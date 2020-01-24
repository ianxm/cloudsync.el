;;; cloudsync.el --- Share a file between locations by storing it on a cloud service -*- lexical-binding: t -*-

;; Copyright (C) 2020 Ian Martins

;; Author: Ian Martins <ianxm@jhu.edu>
;; URL: https://github.com/ianxm/emacs-cloudsync
;; Version: 0.0.1
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

;; TODO build up completion message?
;; TODO verify oauth flow?
;; TODO try to reduce global variables?

(defgroup cloudsync nil
  "Options for customizing CloudSync."
  :group 'comm
  :tag "CloudSync")

(defcustom cloudsync-files nil
  "An alist of files that can be synced, and their cloud service configuration.

Each list entry should look like:

  '(filename cloud-service . cloud-file)

For example:

  '(\"~/.emacs\" s3 . \"s3://mybucketname/.emacs\")"
  :type '(alist :key-type file :value-type (cons string symbol))
  :group 'cloudsync)

(defconst cloudsync-ancestor-dir (file-name-as-directory
                                  (concat (file-name-as-directory user-emacs-directory) "cloudsync"))
  "The directory where we save ancestor files.
Every time we push a file to the cloud we keep a copy of it here.  We use it to
determine which changes, if any, were made to the cloud file from
another machine.")

(defconst cloudsync-tempfile-prefix "cloudsync"
  "The prefix to use when we save cloud files to the local filesystem.")

(defvar cloudsync-backends '((s3 . (cloudsync--fetch-s3 . cloudsync--push-s3)))
  "An alist of cloud service backends.

Each entry looks like:

  (symbol . (fetch-fcn . push-fcn))")

(defvar cloudsync-window-config nil
  "Holds the window configuration so it can be restored after exiting ediff.")
(make-variable-buffer-local 'cloudsync-window-config)

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
  "The names to use for buffers during ediff-merge.")

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
  "Do a \"fetch, merge push\" to sync a local file to the cloud.

All parameters are optional.  If none are given, the LOCAL-FILE can
be chosen from one configured in the `cloudsync-files' variable
and CLOUD-SERVICE and CLOUD-FILE will be pulled from there.

If only LOCAL-FILE is given, CLOUD-SERVICE and CLOUD-FILE will be
looked up in `cloudsync-files'.

LOCAL-FILE is the file on the local filesystem to be synced.

CLOUD-SERVICE is the symbol for the cloud service to which this
file is synced.  It must match a symbol in `cloudsync-backends'.

CLOUD-FILE is the name of the file within the cloud service."
  (interactive)

  (cloudsync--fill-in-params)

  ;; check for local file
  (if (not (file-exists-p local-file))
      (error "File not found: %s" local-file))

  (unless (file-exists-p cloudsync-ancestor-dir)
    (make-directory cloudsync-ancestor-dir)
    (set-file-modes cloudsync-ancestor-dir #o700))

  (setq cloudsync-local-file local-file)
  (setq cloudsync-cloud-file cloud-file)
  (setq cloudsync-cloud-service cloud-service)
  (setq cloudsync-ancestor-file (concat cloudsync-ancestor-dir (file-name-nondirectory cloudsync-local-file)))
  (setq cloudsync-remote-file (make-temp-file cloudsync-tempfile-prefix))

  ;; check for ancestor
  (let* ((ancestor-file-p (file-exists-p cloudsync-ancestor-file)))
    (condition-case err
        (cloudsync-fetch-overwrite cloudsync-remote-file cloudsync-cloud-service cloudsync-cloud-file)
      (error
       (delete-file cloudsync-remote-file)
       (setq cloudsync-remote-file nil)
       (error (error-message-string err))))

    (cond ((cloudsync--diff cloudsync-local-file cloudsync-remote-file)
            ;; if the local file matches the remote file, do nothingn
            (message "No change; not uploading"))

           ((and ancestor-file-p (cloudsync--diff cloudsync-ancestor-file cloudsync-remote-file))
            ;; if there's an ancestor-file which matches remote-file, no need to merge, just push the update
            (cloudsync--finish))

           (t
            ;; else merge and push
            (add-hook 'ediff-startup-hook #'cloudsync--done-maybe)
            (setq cloudsync-ediff-buffer-names '("local" "remote" "merge" "ancestor"))
            (add-hook 'ediff-prepare-buffer-hook #'cloudsync--set-ediff-buffer-names)
            (setq cloudsync-window-config (current-window-configuration))
            (if ancestor-file-p
                (ediff-merge-files-with-ancestor cloudsync-local-file cloudsync-remote-file cloudsync-ancestor-file)
              (ediff-merge-files cloudsync-local-file cloudsync-remote-file))))))

(defun cloudsync--set-ediff-buffer-names ()
  (message "here %s %s" (buffer-name) cloudsync-ediff-buffer-names)
  (rename-buffer (pop cloudsync-ediff-buffer-names)))

;;;###autoload
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
      (funcall (car backend) local-file cloud-file))))

;;;###autoload
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
      (funcall (cdr backend) local-file cloud-file))))

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
        (message "Merge had conflicts -- resolve them with ediff")
        (add-hook 'ediff-cleanup-hook #'cloudsync--done-hopefully))
    ;; overwrite local.txt
    (message "No conflicts, merge was successful")
    (cloudsync--finish)
    (cloudsync--clean-up)))

(defun cloudsync--done-hopefully ()
  "If all conflicts have been resolved, complete the merge.
This is called after the user quits from the ediff interface.  If
any merge conflicts remain, do not upload the file."
  (remove-hook 'ediff-cleanup-hook #'cloudsync--done-hopefully)
  (when (cloudsync--merge-has-conflicts)
      (cloudsync--clean-up)
      (error "Conflicts not merged, not uploading"))
  ;; overwrite local.txt
  (message "Merge was successful")
  (cloudsync--finish)
  (cloudsync--clean-up))

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

(defun cloudsync--finish ()
  "Overwrite the ancestor file and cloud file."
  ;; if there is a merge buffer, write it to the local-file
  (if ediff-buffer-C
      (with-current-buffer ediff-buffer-C
        (write-region (point-min) (point-max) cloudsync-local-file nil)))

  ;; overwrite ancestor-file
  (copy-file cloudsync-local-file cloudsync-ancestor-file t)
  (set-file-modes cloudsync-ancestor-file #o600)

  ;; overwrite cloud-file
  (cloudsync-push-overwrite cloudsync-local-file
                            cloudsync-cloud-service
                            cloudsync-cloud-file)
  (add-hook 'ediff-quit-hook #'cloudsync--upload-message t))

(defun cloudsync--upload-message ()
  (message "Uploaded to the cloud")
  (remove-hook 'ediff-quit-hook #'cloudsync--upload-message))

;; --- backends

(defgroup cloudsync-s3 nil
  "Options for customizing how CloudSync connects to S3."
  :group 'cloudsync
  :tag "CloudSync S3")

(defcustom cloudsync-s3-enable-sse t
  "If t, enable server side encryption (uses AES256)."
  :type 'boolean
  :group 'cloudsync-s3
  :tag "CloudSync S3 Enable SSE")

(defcustom cloudsync-s3-profile nil
  "If set, use the given profile when accessing S3."
  :type 'string
  :group 'cloudsync-s3
  :tag "CloudSync S3 Profile")

(defcustom cloudsync-s3-region nil
  "If set, use the given region when accessing S3."
  :type 'string
  :group 'cloudsync-s3
  :tag "CloudSync S3 Region")

(defun cloudsync--s3-params ()
  "Get the AWS cli params that allow us to override the default profile and region."
  (let ((profile (if cloudsync-s3-profile (concat "--profile " cloudsync-s3-profile " ") ""))
        (region (if cloudsync-s3-region (concat "--region " cloudsync-s3-region " ") "")))
    (concat profile region)))

(defun cloudsync--fetch-s3 (fname cloud-file)
  "Fetch from S3 and save to the local filesystem as FNAME.
CLOUD-FILE is the S3 object, which should like \"s3://bucketname/path/filename.ext\"."
  (with-temp-buffer
    (let* ((params (cloudsync--s3-params))
           (command (format "aws %s s3 cp %s %s" params cloud-file fname))
           (success (call-process-shell-command command nil t)))
      (if (= 0 success)
          (message "Feched file from S3: %s" cloud-file)
        (error "Problem downloading from S3: %s" (buffer-substring (point-min) (point-max)))))))

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

(provide 'cloudsync)

(provide 'cloudsync)

;;; cloudsync.el ends here
