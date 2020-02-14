(require 'cloudsync)
(require 'cloudsync-testparams)

(defconst cloudsync--test-local-file (concat cloudsync--local-workingdir "file.txt"))
(defconst cloudsync--test-ancestor-file (concat cloudsync-ancestor-dir "file.txt"))
(defconst cloudsync--test-s3-cloud-file (concat cloudsync--s3-testdir "file.txt"))
(defconst cloudsync--test-goog-cloud-file (concat cloudsync--goog-testdir "file.txt"))

;; -- helpers

(defun cloudsync--setup (orig-local-file orig-ancestor-file cloud-service orig-cloud-file test-cloud-file)
  "Set up for a test.  Put files in place.
ORIG-LOCAL-FILE is the file with local file content.
ORIG-ANCESTOR-FILE is the file with ancestor file content.
CLOUD-SERVICE is the cloud service to use
ORIG-CLOUD-FILE is the file with cloud file content
TEST-CLOUD-FILE is the filename to use for the cloud file request"
  ;; stage or delete ancestor file
  (cond
   ;; file exists and we don't want it, delete it
   ((and (null orig-ancestor-file) (file-exists-p cloudsync--test-ancestor-file))
    (delete-file cloudsync--test-ancestor-file))
   ;; file doesn't exist and we don't want it, do nothing
   ((and (null orig-ancestor-file) (not (file-exists-p cloudsync--test-ancestor-file))))
   ;; we want the file, copy it
   (t
    (copy-file orig-ancestor-file cloudsync--test-ancestor-file t)
    (set-file-modes cloudsync--test-ancestor-file #o600)))

  ;; stage cloud file
  (if (null orig-cloud-file)
      (cloudsync-delete cloud-service test-cloud-file)
    (copy-file orig-cloud-file cloudsync--test-local-file t)
    (cloudsync-push-overwrite cloudsync--test-local-file cloud-service test-cloud-file))

  ;; stage or delete local file, unless it's the same as the cloud
  ;; file since we already copied the cloud file to the local file's
  ;; path
  (cond
   ;; file exists and we don't want it, delete it
   ((and (null orig-local-file) (file-exists-p cloudsync--test-local-file))
    (delete-file cloudsync--test-local-file))
   ;; file doesn't exist and we don't want it, do nothing
   ((and (null orig-local-file) (not (file-exists-p cloudsync--test-local-file))))
   ;; we want the file and we didn't already copy it, copy it
   ((not (string= orig-local-file orig-cloud-file))
    (copy-file orig-local-file cloudsync--test-local-file t)))

  (message "setup complete"))

;; no differences, don't do anything
(defun cloudsync--test-no-diff (cloud-service cloud-file)
  ;; setup
  (cloudsync--setup
   (concat cloudsync--local-testdir "original.txt")
   (concat cloudsync--local-testdir "original.txt")
   cloud-service
   (concat cloudsync--local-testdir "original.txt")
   cloud-file)
  ;; call
  (cloudsync-sync cloudsync--test-local-file cloud-service cloud-file))

;; no remote changes, just push local
(defun cloudsync--test-no-remote-changes (cloud-service cloud-file)
  ;; setup
  (cloudsync--setup
   (concat cloudsync--local-testdir "changed_1_local.txt")
   (concat cloudsync--local-testdir "original.txt")
   cloud-service
   (concat cloudsync--local-testdir "original.txt")
   cloud-file)
  ;; call
  (cloudsync-sync cloudsync--test-local-file cloud-service cloud-file))

;; no remote file, just push local
(defun cloudsync--test-no-remote-file (cloud-service cloud-file)
  ;; setup
  (cloudsync--setup
   (concat cloudsync--local-testdir "changed_1_local.txt")
   (concat cloudsync--local-testdir "original.txt")
   cloud-service
   nil
   cloud-file)
  ;; call
  (cloudsync-sync cloudsync--test-local-file cloud-service cloud-file))

;; no local changes, just pull remote
(defun cloudsync--test-no-local-changes (cloud-service cloud-file)
  ;; setup
  (cloudsync--setup
   (concat cloudsync--local-testdir "original.txt")
   (concat cloudsync--local-testdir "original.txt")
   cloud-service
   (concat cloudsync--local-testdir "changed_3_remote.txt")
   cloud-file)
  ;; call
  (cloudsync-sync cloudsync--test-local-file cloud-service cloud-file))

;; no local file, just pull remote
(defun cloudsync--test-no-local-file (cloud-service cloud-file)
  ;; setup
  (cloudsync--setup
   nil
   nil
   cloud-service
   (concat cloudsync--local-testdir "original.txt")
   cloud-file)
  ;; call
  (cloudsync-sync cloudsync--test-local-file cloud-service cloud-file))

;; fast forward (both changed but no conflict; needs ancestor)
(defun cloudsync--test-fast-forward (cloud-service cloud-file)
  ;; setup
  (cloudsync--setup
   (concat cloudsync--local-testdir "changed_1_local.txt")
   (concat cloudsync--local-testdir "original.txt")
   cloud-service
   (concat cloudsync--local-testdir "changed_3_remote.txt")
   cloud-file)
  ;; call
  (cloudsync-sync cloudsync--test-local-file cloud-service cloud-file))

;; merge (needs ancestor)
(defun cloudsync--test-merge (cloud-service cloud-file)
  ;; setup
  (cloudsync--setup
   (concat cloudsync--local-testdir "changed_13_local.txt")
   (concat cloudsync--local-testdir "original.txt")
   cloud-service
   (concat cloudsync--local-testdir "changed_3_remote.txt")
   cloud-file)
  ;; call
  (cloudsync-sync cloudsync--test-local-file cloud-service cloud-file))

;; no ancestor merge
(defun cloudsync--test-no-ancestor-merge (cloud-service cloud-file)
  ;; setup
  (cloudsync--setup
   (concat cloudsync--local-testdir "changed_1_local.txt")
   nil
   cloud-service
   (concat cloudsync--local-testdir "changed_3_remote.txt")
   cloud-file)
  ;; call
  (cloudsync-sync cloudsync--test-local-file cloud-service cloud-file))

(provide 'cloudsync-testhelpers)
