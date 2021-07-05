;;; cloudsync.el --- Share a file between locations by storing it on a cloud service -*- lexical-binding: t -*-

;; Copyright (C) 2020 Ian Martins

;; Author: Ian Martins <ianxm@jhu.edu>
;; URL: https://github.com/ianxm/cloudsync.el
;; Version: 0.0.7
;; Keywords: comm
;; Package-Requires: ((emacs "25.2"))

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; cloudsync.el share a file between locations by storing it on a
;; cloud service.

;;; Code:

(require 'ediff-init)
(require 'ediff-util)

;; TODO try to reduce global variables?

(defgroup cloudsync nil
  "Options for customizing CloudSync."
  :group 'comm
  :tag "CloudSync")

(defcustom cloudsync-files nil
  "An alist of files that can be synced, and their cloud service configuration.

Each list entry should look like:

  '(filename backend . cloud-file)

For example:

  '((\"~/.emacs\" s3 . \"s3://mybucketname/.emacs\")
    (\"~/diary\" rclone . \"gdrive:diary\"))

The first configuration item allows you to sync your .emacs file
to S3 using the AWS CLI.  You must have already created and
configured your S3 bucket and installed and configured the AWS
CLI.

The second configuration item allows you to sync your diary file
to Google Drive (or any other cloud service rclone supports).
You must have already installed and configured rclone."
  :type '(alist :key-type file :value-type (cons string symbol))
  :group 'cloudsync)

(defcustom cloudsync-confirm-before-overwrite nil
  "If t, always show a \"yes or no\" prompt before updating the local or cloud file."
  :type 'boolean
  :group 'cloudsync)

(defconst cloudsync-ancestor-dir (file-name-as-directory
                                  (concat (file-name-as-directory user-emacs-directory) "cloudsync"))
  "The directory where we save ancestor files.
Every time we push a file to the cloud we keep a copy of it here.
We use it to determine which changes, if any, were made to the
cloud file from another machine.")

(defconst cloudsync-tempfile-dir (file-name-as-directory (concat temporary-file-directory "cloudsync"))
  "The temp subdirectory to put cloud files when we copy them to the local filesystem.")

(defconst cloudsync-backends '((s3 . (cloudsync--fetch-s3 cloudsync--push-s3 cloudsync--delete-s3))
                               (rclone . (cloudsync--fetch-rclone cloudsync--push-rclone cloudsync--delete-rclone)))
  "An alist of cloud service backends.

Each entry looks like:

  (symbol . (fetch-fcn push-fcn delete-fcn))")

(defvar cloudsync-window-config nil
  "Holds the window configuration so it can be restored after exiting ediff.")

(defvar cloudsync-cloud-service nil
  "An identifier for the cloud service.")

(defvar cloudsync-local-file nil
  "The name of the local file to be synced.")

(defvar cloudsync-remote-file nil
  "The name of the temp file that holds the cloud file after it is pulled to the local filesystem.")

(defvar cloudsync-ancestor-file nil
  "A copy of the local file the last time it was uploaded.")

(defvar cloudsync-cloud-file nil
  "A cloud service specific identifier for the cloud file.")

(defvar cloudsync-ediff-buffer-names nil
  "The names to use for buffers during ‘ediff-merge’.")

(defvar cloudsync-message nil
  "The message to give when exiting ediff to summarize what was done.")

(define-error 'cloudsync--file-not-found
  "The cloudfile was not found" 'file-missing)

;; TODO not sure if this will work on windows
(defun cloudsync--diff (file1 file2)
  "Return t if FILE1 and FILE2 are identical."
  (if (or (null file1) (null file2))
      (error "One of the files to diff is null: (%s, %s)" file1 file2)
    (with-temp-buffer
      (let ((exit-status (call-process-shell-command (format "diff -q %s %s" file1 file2) nil t)))
        (cond ((= exit-status 0) t)            ; same
              ((= exit-status 1) nil)          ; different
              ((= exit-status 2) (error "Problem calling diff: %s" (buffer-substring (point-min) (point-max)))))))))

(defmacro cloudsync--fill-in-params ()
  "Fill in optional params that weren't passed in.
This sets `local-file' `cloud-service' and `cloud-file' for the
caller."
  `(progn
     (if (not local-file)
         (if (not cloudsync-files)
             (error "You must configure the files you want to sync.  See `cloudsync-files' for details")
       (setq local-file (completing-read "Local file: " (mapcar (lambda (x) (car x)) cloudsync-files)))))
     (when (not cloud-service)
       (let ((config-item (assoc local-file cloudsync-files)))
         (if (not config-item)
             (error "The specified file is not configured.  See `cloudsync-files' for details")
           (setq cloud-service (cadr config-item)
               cloud-file (cddr config-item)))))))

;;;###autoload
(defun cloudsync-sync (&optional local-file cloud-service cloud-file)
  "Do a \"fetch, merge, push\" to sync a local file to the cloud.

All parameters are optional.  If none are given, the LOCAL-FILE can
be chosen from one configured in the `cloudsync-files' variable
and CLOUD-SERVICE and CLOUD-FILE will be pulled from there.

If only LOCAL-FILE is given, CLOUD-SERVICE and CLOUD-FILE will be
looked up in `cloudsync-files'.

LOCAL-FILE is the file on the local filesystem to be synced.

CLOUD-SERVICE is the symbol for the cloud service to which this
file is synced.  It must match a symbol in `cloudsync-backends'.

CLOUD-FILE is the name of the file within the cloud service.

These are the possible outcomes:

| situation                    | result         |
|------------------------------+----------------|
| local matches cloud          | do nothing     |
| no cloud file                | push local     |
| no local file                | pull cloud     |
| ancestor matches cloud       | push local     |
| ancestor matches local       | pull cloud     |
| no merge conflicts           | merge and push |
| merge matches cloud          | merge no push  |
| merge conflicts resolved     | merge and push |
| merge conflicts not resolved | do nothing     |"
  (interactive)

  (cloudsync--fill-in-params)

  (unless (file-exists-p cloudsync-ancestor-dir)
    (make-directory cloudsync-ancestor-dir)
    (set-file-modes cloudsync-ancestor-dir #o700))
  ;; TODO consider naming the temp dir with the user's name and making it private
  (unless (file-exists-p cloudsync-tempfile-dir)
    (make-directory cloudsync-tempfile-dir))

  (setq cloudsync-local-file local-file)
  (setq cloudsync-cloud-file cloud-file)
  (setq cloudsync-cloud-service cloud-service)
  (setq cloudsync-ancestor-file (concat cloudsync-ancestor-dir (file-name-nondirectory cloudsync-local-file)))
  (setq cloudsync-remote-file (concat cloudsync-tempfile-dir (file-name-nondirectory cloudsync-local-file)))

  ;; check for ancestor
  (let* ((ancestor-file-p (file-exists-p cloudsync-ancestor-file))
         (local-file-p (file-exists-p local-file))
         remote-file-p)
    (condition-case err
        (progn (cloudsync-fetch-overwrite cloudsync-remote-file cloudsync-cloud-service cloudsync-cloud-file)
               (setq remote-file-p t))
      (cloudsync--file-not-found) ; do nothing
      (error
       ;; problem accessing cloud service
       (if remote-file-p
           (delete-file cloudsync-remote-file))
       (setq remote-file-p nil
             cloudsync-remote-file nil)
       (error (error-message-string err))))

    (cond ((and (not local-file-p) (not remote-file-p))
           ;; no local file and no remote file, do nothing
           (error "Neither local or remote file exists, nothing to sync"))

          ((and remote-file-p
                local-file-p
                (cloudsync--diff cloudsync-local-file cloudsync-remote-file))
           ;; if the local file and remote file both exist and the
           ;; local file matches the remote file, just update ancestor
           (if remote-file-p
               (delete-file cloudsync-remote-file))
           (copy-file cloudsync-local-file cloudsync-ancestor-file t)
           (set-file-modes cloudsync-ancestor-file #o600)
           (message "No changes; no update needed"))

          ((or (not remote-file-p)
               (and ancestor-file-p
                    (cloudsync--diff cloudsync-ancestor-file cloudsync-remote-file)))
            ;; if there's no remote file, or there is an ancestor file
            ;; and remote file and they match, no need to merge, just
            ;; push the update
            (cloudsync--save-changes)
            (if (not remote-file-p)
                (message "No remote file, pushed local file")
              (delete-file cloudsync-remote-file)
              (message "No remote changes, pushed local changes")))

          ((or (not local-file-p)
               (and ancestor-file-p
                    (cloudsync--diff cloudsync-ancestor-file cloudsync-local-file)))
            ;; if there's no local file, or there is an ancestor file
            ;; which matches local file, no need to merge, just pull
            ;; the update
           (cloudsync-fetch-overwrite cloudsync-local-file
                                      cloudsync-cloud-service
                                      cloudsync-cloud-file)
           ;; overwrite ancestor-file and remove the temp file
           (copy-file cloudsync-local-file cloudsync-ancestor-file t)
           (set-file-modes cloudsync-ancestor-file #o600)
           (if (file-exists-p cloudsync-remote-file)
               (delete-file cloudsync-remote-file))
           (if local-file-p
               (message "No local changes, pulled remote changes")
             (message "No local file, pulled remote file")))

          (t
            ;; else merge and push
            (add-hook 'ediff-startup-hook #'cloudsync--done-maybe)
            (add-hook 'ediff-quit-hook #'cloudsync--exit-message t)
            (setq cloudsync-ediff-buffer-names '("local" "remote" "merge" "ancestor"))
            (add-hook 'ediff-prepare-buffer-hook #'cloudsync--set-ediff-buffer-names)
            (setq cloudsync-window-config (current-window-configuration))
            (if ancestor-file-p
                (ediff-merge-files-with-ancestor cloudsync-local-file cloudsync-remote-file cloudsync-ancestor-file)
              (ediff-merge-files cloudsync-local-file cloudsync-remote-file)))))
  nil)

(defun cloudsync--set-ediff-buffer-names ()
  "Set ediff buffer names.
This is called for each buffer by ediff during initialization
after it has created and layed out the buffers (A, B, C).  We use
it to modify buffer names to \"local, remote, merge\"."
  (rename-buffer (pop cloudsync-ediff-buffer-names)))

(defun cloudsync-fetch-overwrite (&optional local-file cloud-service cloud-file)
  "Fetch a file from the cloud.  OVERWRITES LOCAL-FILE!

All parameters are optional.  If none are given, the LOCAL-FILE can
be chosen from one configured in the `cloudsync-files' variable
and CLOUD-SERVICE and CLOUD-FILE will be pulled from there.

If only LOCAL-FILE is given, CLOUD-SERVICE and CLOUD-FILE will be
looked up in `cloudsync-files'.

LOCAL-FILE is the file on the local filesystem to be written.

CLOUD-SERVICE is the symbol for the cloud service to which this
file is synced.  It must match a symbol in `cloudsync-backends'.

CLOUD-FILE is the name of the file within the cloud service to
fetch."
  (interactive)

  (cloudsync--fill-in-params)

  (let ((backend (alist-get cloud-service cloudsync-backends)))
    (if (null backend)
        (error "Unknown cloudsync backend: %s" cloud-service)
      (if (or (not (file-exists-p local-file))
              (cloudsync--confirm local-file))
          (funcall (nth 0 backend) local-file cloud-file))))
  nil)

(defun cloudsync-push-overwrite (&optional local-file cloud-service cloud-file)
  "Push a file to the cloud. OVERWRITES CLOUD-FILE!

All parameters are optional.  If none are given, the LOCAL-FILE can
be chosen from one configured in the `cloudsync-files' variable
and CLOUD-SERVICE and CLOUD-FILE will be pulled from there.

If only LOCAL-FILE is given, CLOUD-SERVICE and CLOUD-FILE will be
looked up in `cloudsync-files'.

LOCAL-FILE is the file on the local filesystem to be pushed.

CLOUD-SERVICE is the symbol for the cloud service to which this
file is synced.  It must match a symbol in `cloudsync-backends'.

CLOUD-FILE is the name of the file within the cloud service to
write."
  (interactive)

  (cloudsync--fill-in-params)

  (let ((backend (alist-get cloud-service cloudsync-backends)))
    (if (null backend)
        (error "Unknown cloudsync backend: %s" cloudsync-cloud-service)

      (if (cloudsync--confirm cloud-file)
          (funcall (nth 1 backend) local-file cloud-file))))
  nil)

(defun cloudsync-delete (cloud-service cloud-file &optional ignore-failures)
  "Delete a file from the cloud.

CLOUD-SERVICE is the symbol for the cloud service to which this
file is synced.  It must match a symbol in `cloudsync-backends'.

CLOUD-FILE is the name of the file within the cloud service to
write.

IGNORE-FAILURES if set, ignore error messages.  This is useful
when deleting a file that may not be there on GDrive."
  (interactive)

  (let ((backend (alist-get cloud-service cloudsync-backends)))
    (if (null backend)
        (error "Unknown cloudsync backend: %s" cloudsync-cloud-service)
      (funcall (nth 2 backend) cloud-file ignore-failures)))
  nil)

(defun cloudsync--merge-has-conflicts ()
  "Return t if any conflicts are found in the merge buffer, else nil."
  (with-current-buffer ediff-buffer-C
    (goto-char (point-min))
    (if (search-forward "<<<" nil t) t nil)))

(defun cloudsync--done-maybe ()
  "If there are no conflicts, complete the merge.
This is called after ediff has computed the merge, but before the
user is presented with the ediff interface."
  (remove-hook 'ediff-startup-hook #'cloudsync--done-maybe)
  (if (cloudsync--merge-has-conflicts)
      (progn
        (message "Merge had conflicts. Resolve them with ediff. Quit ediff to continue.")
        (add-hook 'ediff-cleanup-hook #'cloudsync--done-hopefully))
    ;; overwrite local-file
    (if (cloudsync--save-changes)
        (message "Both changed but no conflicts, updated local and remote")
      (message "No local changes after merge, pulled remote changes")) ; this shouldn't happen
    ;; not sure this is fine to do.  ediff just started up and now
    ;; we're killing it without running `ediff-really-quit' to let it
    ;; shutdown gracefully.  There may be side effects or this may not
    ;; work on some versions.
    (cloudsync--clean-up)))

(defun cloudsync--done-hopefully ()
  "If all conflicts have been resolved, complete the merge.
This is called after the user quits from the ediff interface.  If
any merge conflicts remain, do not upload the file."
  (remove-hook 'ediff-cleanup-hook #'cloudsync--done-hopefully)
  (cond ((cloudsync--merge-has-conflicts)
         ;; conflicts not resolved
         (setq cloudsync-message "Both changed and conflicts were not resolved, not updating")
         (cloudsync--clean-up))
        (t
         ;; conflicts resolved
         (if (cloudsync--save-changes)
             (setq cloudsync-message "Both changed and conflicts were resolved, updated local and remote")
           (setq cloudsync-message "No local changes after merge, pulled remote changes"))
         (cloudsync--clean-up))))

(defun cloudsync--confirm (fname)
  "If `confirm-before-overwrite' is enabled, check if we should write.
FNAME is the file to write.
Return t if we are allowed to write."
  (if cloudsync-confirm-before-overwrite
      (yes-or-no-p (concat "Overwrite " fname "?"))
    t))

(defun cloudsync--save-changes ()
  "Save changes to the local and cloud files.
Overwrite the local file with merge changes.  Overwrite the
ancestor file and cloud file if necessary.  Return t if we update
the remote, else return nil."
  ;; if there is a merge buffer, write it to the local-file
  (if (and ediff-buffer-C
           (cloudsync--confirm cloudsync-local-file))
      (with-current-buffer ediff-buffer-C
        (write-region (point-min) (point-max) cloudsync-local-file nil)))

  (if (and (file-exists-p cloudsync-remote-file)
           (cloudsync--diff cloudsync-local-file cloudsync-remote-file))
      nil
    ;; overwrite ancestor-file
    (copy-file cloudsync-local-file cloudsync-ancestor-file t)
    (set-file-modes cloudsync-ancestor-file #o600)

    ;; overwrite cloud-file
    (cloudsync-push-overwrite cloudsync-local-file
                              cloudsync-cloud-service
                              cloudsync-cloud-file)
    t))

(defun cloudsync--clean-up ()
  "Delete temp file, clean up ediff buffers and reset the window configuration."

  ;; delete temp remote-file
  (if (file-exists-p cloudsync-remote-file)
      (delete-file cloudsync-remote-file))

  ;; clean up ediff buffers
  (ediff-janitor nil nil)
  (if ediff-buffer-C
      (kill-buffer ediff-buffer-C))
  (if ediff-control-buffer
      (kill-buffer ediff-control-buffer))

  (remove-hook 'ediff-prepare-buffer-hook #'cloudsync--set-ediff-buffer-names)

  ;; reset window configuration
  (set-window-configuration cloudsync-window-config)
  (setq cloudsync-window-config nil))

(defun cloudsync--exit-message ()
  "Set message when CloudSync exists.
If we start ediff then we have to set the final status message
here to summarize what we did since any message posted at the end
of the sync function would have been overwritten by ediff."
  (message cloudsync-message)
  (setq cloudsync-message nil)
  (remove-hook 'ediff-quit-hook #'cloudsync--exit-message))

;; --- s3 backend

(defgroup cloudsync-s3 nil
  "Options for customizing how CloudSync connects to S3 via the AWS CLI."
  :group 'cloudsync
  :tag "CloudSync AWS S3")

(defcustom cloudsync-s3-profile nil
  "If set, use the given profile when accessing S3 via the AWS CLI."
  :type 'string
  :group 'cloudsync-s3
  :tag "CloudSync AWS S3 Profile")

(defcustom cloudsync-s3-region nil
  "If set, use the given region when accessing S3 via the AWS CLI."
  :type 'string
  :group 'cloudsync-s3
  :tag "CloudSync AWS S3 Region")

(defcustom cloudsync-s3-enable-sse t
  "If t, enable server side encryption (uses AES256) when accessing S3 via the AWS CLI."
  :type 'boolean
  :group 'cloudsync-s3
  :tag "CloudSync AWS S3 Enable SSE")

(defun cloudsync--s3-params ()
  "Get the AWS CLI params that allow us to override the default profile and region."
  (let ((profile (if cloudsync-s3-profile (concat "--profile " cloudsync-s3-profile " ") ""))
        (region (if cloudsync-s3-region (concat "--region " cloudsync-s3-region " ") "")))
    (concat profile region)))

(defun cloudsync--fetch-s3 (fname cloud-file)
  "Fetch from S3 and save to the local filesystem as FNAME.
CLOUD-FILE is the S3 object, which should like \"s3://bucketname/path/filename.ext\"."
  (with-temp-buffer
    (let* ((params (cloudsync--s3-params))
           (command (format "aws %s s3 cp %s %s" params cloud-file fname))
           (success (call-process-shell-command command nil t))
           error-msg)
      (if (= 0 success)
          (message "Fetched file from S3: %s" cloud-file)
        (setq error-msg (buffer-substring (point-min) (point-max)))
        (if (string-match "Key .* does not exist" error-msg)
            (signal 'cloudsync--file-not-found error-msg) ; this is the initial sync
          (error "Problem downloading from S3: %s" error-msg))))))

(defun cloudsync--push-s3 (fname cloud-file)
  "Push local file FNAME to S3 as CLOUD-FILE.
CLOUD-FILE looks like \"s3://bucketname/path/filename.ext\"."
  (with-temp-buffer
    (let* ((params (cloudsync--s3-params))
           (sse (if cloudsync-s3-enable-sse "--sse AES256 " ""))
           (command (format "aws %s s3 cp %s %s %s" params sse fname cloud-file))
           (success (call-process-shell-command command nil t)))
      (if (= 0 success)
          (message "Pushed file to S3: %s" cloud-file)
        (error "Problem uploading to S3: %s" (buffer-substring (point-min) (point-max)))))))

(defun cloudsync--delete-s3 (cloud-file ignore-failure)
  "Delete CLOUD-FILE from S3.
CLOUD-FILE looks like \"s3://bucketname/path/filename.ext\"."
  (with-temp-buffer
    (let* ((params (cloudsync--s3-params))
           (command (format "aws %s s3 rm %s" params cloud-file))
           (success (call-process-shell-command command nil t)))
      (if (or ignore-failure (= 0 success))
          (message "Deleted file from S3: %s" cloud-file)
        (error "Problem deleting from S3: %s" (buffer-substring (point-min) (point-max)))))))


;; --- rclone backend (Google Drive, DropBox, MS OneDrive, AWS S3, etc)

(defun cloudsync--fetch-rclone (fname cloud-file)
  "Fetch using rclone and save to the local filesystem as FNAME.
CLOUD-FILE is the full remote path, which should like
\"remote:path/filename.ext\".  The local and remote paths may
differ but the filenames must be the same."
  (with-temp-buffer
    (let ((local-fname (file-name-nondirectory fname))
          (remote-fname (file-name-nondirectory cloud-file)))
      (unless (string= local-fname remote-fname)
        (error (format "Local and remote filenames differ: %s %s" local-fname remote-fname))))
    (let* ((command (format "rclone copy %s %s" cloud-file (file-name-directory fname)))
           (success (call-process-shell-command command nil t))
           error-msg)
      (if (= 0 success)
          (message "Fetched file using rclone: %s" cloud-file)
        (setq error-msg (buffer-substring (point-min) (point-max)))
        (if (string-match ".* error reading source directory: directory not found" error-msg)
            (signal 'cloudsync--file-not-found error-msg) ; this is the initial sync
          (error "Problem downloading using rclone: %s" error-msg))))))

(defun cloudsync--push-rclone (fname cloud-file)
  "Push local file FNAME using rclone to CLOUD-FILE.
CLOUD-FILE looks like \"remote:path/filename.ext\".  The local
and remote paths may differ but the filenames must be the same."
  (with-temp-buffer
    (let ((local-fname (file-name-nondirectory fname))
          (remote-fname (file-name-nondirectory cloud-file)))
      (unless (string= local-fname remote-fname)
        (error (format "Local and remote filenames differ: %s %s" local-fname remote-fname))))
    (let* ((command (format "rclone copy %s %s" fname (file-name-directory cloud-file)))
           (success (call-process-shell-command command nil t)))
      (if (= 0 success)
          (message "Pushed file using rclone: %s" cloud-file)
        (error "Problem uploading using rclone: %s" (buffer-substring (point-min) (point-max)))))))

(defun cloudsync--delete-rclone (cloud-file ignore-failure)
  "Delete CLOUD-FILE from rclone path.
CLOUD-FILE looks like \"remote:path/filename.ext\".  The local
and remote paths may differ but the filenames must be the same."
  (with-temp-buffer
    (let* ((command (format "rclone delete %s" cloud-file))
           (success (call-process-shell-command command nil t)))
      (if (or ignore-failure (= 0 success))
          (message "Deleted file using rclone: %s" cloud-file)
        (error "Problem deleting using rclone: %s" (buffer-substring (point-min) (point-max)))))))

(provide 'cloudsync)

;;; cloudsync.el ends here
