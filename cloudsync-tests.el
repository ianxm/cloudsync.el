(require 'cloudsync)
(require 'cloudsync-testparams)

;; test filenames

(defconst cloudsync--test-local-file (concat cloudsync--local-workingdir "file.txt")
  "where to put local file")
(defconst cloudsync--test-ancestor-file (concat cloudsync-ancestor-dir "file.txt")
  "where to put ancestor file")
(defconst cloudsync--test-s3-cloud-file (concat cloudsync--s3-testdir "file.txt")
  "URI of cloud file on S3")
(defconst cloudsync--test-gdrive-cloud-file (concat cloudsync--gdrive-testdir "file.txt")
  "URI of cloud file on google drive")

;; note: Tests only cover cases where ediff isn't started.

;; tests

(ert-deftest cloudsync/test-s3-create-from-local ()
  "S3: local is a new file, copy to ancestor and cloud files"
  (let ((cloudsync-always-show-diff nil)
        (cloudsync-confirm-before-overwrite nil))
    (cloudsync--run-test
     "cloudsync/test-s3-create-from-local: original content"
     nil
     's3
     nil
     cloudsync--test-s3-cloud-file
     "cloudsync/test-s3-create-from-local: original content")))

(ert-deftest cloudsync/test-gdrive-create-from-local ()
  "GDrive: local is a new file, copy to ancestor and cloud files"
  (let ((cloudsync-always-show-diff nil)
        (cloudsync-confirm-before-overwrite nil))
    (cloudsync--run-test
     "cloudsync/test-gdrive-create-from-local: original content"
     nil
     'rclone
     nil
     cloudsync--test-gdrive-cloud-file
     "cloudsync/test-gdrive-create-from-local: original content")))

(ert-deftest cloudsync/test-s3-create-from-cloud ()
  "S3: cloud is a new file, copy to local and ancestor files"
  (let ((cloudsync-always-show-diff nil)
        (cloudsync-confirm-before-overwrite nil))
    (cloudsync--run-test
     nil
     nil
     's3
     "cloudsync/test-s3-create-from-cloud: original content"
     cloudsync--test-s3-cloud-file
     "cloudsync/test-s3-create-from-cloud: original content")))

(ert-deftest cloudsync/test-gdrive-create-from-cloud ()
  "GDrive: cloud is a new file, copy to local and ancestor files"
  (let ((cloudsync-always-show-diff nil)
        (cloudsync-confirm-before-overwrite nil))
    (cloudsync--run-test
     nil
     nil
     'rclone
     "cloudsync/test-gdrive-create-from-cloud: original content"
     cloudsync--test-gdrive-cloud-file
     "cloudsync/test-gdrive-create-from-cloud: original content")))

(ert-deftest cloudsync/test-s3-no-change ()
  "S3: no differences, do nothing"
  (let ((cloudsync-always-show-diff nil)
        (cloudsync-confirm-before-overwrite nil))
    (cloudsync--run-test
     "cloudsync/test-s3-no-change: original content"
     "cloudsync/test-s3-no-change: original content"
     's3
     "cloudsync/test-s3-no-change: original content"
     cloudsync--test-s3-cloud-file
     "cloudsync/test-s3-no-change: original content")))

(ert-deftest cloudsync/test-gdrive-no-change ()
  "GDrive: no differences, do nothing"
  (let ((cloudsync-always-show-diff nil)
        (cloudsync-confirm-before-overwrite nil))
    (cloudsync--run-test
     "cloudsync/test-gdrive-no-change: original content"
     "cloudsync/test-gdrive-no-change: original content"
     'rclone
     "cloudsync/test-gdrive-no-change: original content"
     cloudsync--test-gdrive-cloud-file
     "cloudsync/test-gdrive-no-change: original content")))

(ert-deftest cloudsync/test-s3-add-to-local ()
  "S3: add content to local, sync to cloud"
  (let ((cloudsync-always-show-diff nil)
        (cloudsync-confirm-before-overwrite nil))
    (cloudsync--run-test
     "cloudsync/test-s3-add-to-local: original content
new line from local"
     "cloudsync/test-s3-add-to-local: original content"
     's3
     "cloudsync/test-s3-add-to-local: original content"
     cloudsync--test-s3-cloud-file
     "cloudsync/test-s3-add-to-local: original content
new line from local")))

(ert-deftest cloudsync/test-gdrive-add-to-local ()
  "GDrive: add content to local, sync to cloud"
  (let ((cloudsync-always-show-diff nil)
        (cloudsync-confirm-before-overwrite nil))
    (cloudsync--run-test
     "cloudsync/test-gdrive-add-to-local: original content
new line from local"
     "cloudsync/test-gdrive-add-to-local: original content"
     'rclone
     "cloudsync/test-gdrive-add-to-local: original content"
     cloudsync--test-gdrive-cloud-file
     "cloudsync/test-gdrive-add-to-local: original content
new line from local")))

(ert-deftest cloudsync/test-s3-add-to-cloud ()
  "S3: add content to cloud, sync to local"
  (let ((cloudsync-always-show-diff nil)
        (cloudsync-confirm-before-overwrite nil))
    (cloudsync--run-test
     "cloudsync/test-s3-add-to-local: original content"
     "cloudsync/test-s3-add-to-local: original content"
     's3
     "cloudsync/test-s3-add-to-local: original content
new line from cloud"
     cloudsync--test-s3-cloud-file
     "cloudsync/test-s3-add-to-local: original content
new line from cloud")))

(ert-deftest cloudsync/test-gdrive-add-to-cloud ()
  "GDrive: add content to cloud, sync to local"
  (let ((cloudsync-always-show-diff nil)
        (cloudsync-confirm-before-overwrite nil))
    (cloudsync--run-test
     "cloudsync/test-gdrive-add-to-local: original content"
     "cloudsync/test-gdrive-add-to-local: original content"
     'rclone
     "cloudsync/test-gdrive-add-to-local: original content
new line from cloud"
     cloudsync--test-gdrive-cloud-file
     "cloudsync/test-gdrive-add-to-local: original content
new line from cloud")))

(ert-deftest cloudsync/test-s3-add-to-both-nonoverlap ()
  "S3: add non-overlapping content to both, sync both"
  (let ((cloudsync-always-show-diff nil)
        (cloudsync-confirm-before-overwrite nil))
    (cloudsync--run-test
     "cloudsync/test-s3-add-to-both-nonoverlap
local change
original content
"
     "cloudsync/test-s3-add-to-both-nonoverlap
original content
"
     's3
     "cloudsync/test-s3-add-to-both-nonoverlap
original content
cloud change
"
     cloudsync--test-s3-cloud-file
     "cloudsync/test-s3-add-to-both-nonoverlap
local change
original content
cloud change
")))

(ert-deftest cloudsync/test-gdrive-add-to-both-nonoverlap ()
  "GDrive: add non-overlapping content to both, sync both"
  (let ((cloudsync-always-show-diff nil)
        (cloudsync-confirm-before-overwrite nil))
    (cloudsync--run-test
     "cloudsync/test-gdrive-add-to-both-nonoverlap
local change
original content
"
     "cloudsync/test-gdrive-add-to-both-nonoverlap
original content
"
     'rclone
     "cloudsync/test-gdrive-add-to-both-nonoverlap
original content
cloud change
"
     cloudsync--test-gdrive-cloud-file
     "cloudsync/test-gdrive-add-to-both-nonoverlap
local change
original content
cloud change
")))

(ert-deftest cloudsync/test-s3-add-to-both-overlap-same-content ()
  "S3: add overlapping identical content to both, updates ancestor"
  (let ((cloudsync-always-show-diff nil)
        (cloudsync-confirm-before-overwrite nil))
    (cloudsync--run-test
     "cloudsync/test-s3-add-to-both-overlap-same-content
original content
changed by both
"
     "cloudsync/test-s3-add-to-both-overlap-same-content
original content
"
     's3
     "cloudsync/test-s3-add-to-both-overlap-same-content
original content
changed by both
"
     cloudsync--test-s3-cloud-file
     "cloudsync/test-s3-add-to-both-overlap-same-content
original content
changed by both
")))

(ert-deftest cloudsync/test-gdrive-add-to-both-overlap-same-content ()
  "GDrive: add overlapping identical content to both, updates ancestor"
  (let ((cloudsync-always-show-diff nil)
        (cloudsync-confirm-before-overwrite nil))
    (cloudsync--run-test
     "cloudsync/test-gdrive-add-to-both-overlap-same-content
original content
changed by both
"
     "cloudsync/test-gdrive-add-to-both-overlap-same-content
original content
"
     'rclone
     "cloudsync/test-gdrive-add-to-both-overlap-same-content
original content
changed by both
"
     cloudsync--test-gdrive-cloud-file
     "cloudsync/test-gdrive-add-to-both-overlap-same-content
original content
changed by both
")))

(ert-deftest cloudsync/test-s3-remove-from-local ()
  "S3: remove content from local, sync to cloud"
  (let ((cloudsync-always-show-diff nil)
        (cloudsync-confirm-before-overwrite nil))
    (cloudsync--run-test
     "cloudsync/test-s3-remove-from-local: original content"
     "cloudsync/test-s3-remove-from-local: original content
remove this"
     's3
     "cloudsync/test-s3-remove-from-local: original content
remove this"
     cloudsync--test-s3-cloud-file
     "cloudsync/test-s3-remove-from-local: original content")))

(ert-deftest cloudsync/test-gdrive-remove-from-local ()
  "GDrive: remove content from local, sync to cloud"
  (let ((cloudsync-always-show-diff nil)
        (cloudsync-confirm-before-overwrite nil))
    (cloudsync--run-test
     "cloudsync/test-gdrive-remove-from-local: original content"
     "cloudsync/test-gdrive-remove-from-local: original content
remove this"
     'rclone
     "cloudsync/test-gdrive-remove-from-local: original content
remove this"
     cloudsync--test-gdrive-cloud-file
     "cloudsync/test-gdrive-remove-from-local: original content")))

(ert-deftest cloudsync/test-s3-remove-from-cloud ()
  "S3: remove content from cloud, sync to local"
  (let ((cloudsync-always-show-diff nil)
        (cloudsync-confirm-before-overwrite nil))
    (cloudsync--run-test
     "cloudsync/test-s3-remove-from-local: original content
remove this"
     "cloudsync/test-s3-remove-from-local: original content
remove this"
     's3
     "cloudsync/test-s3-remove-from-local: original content"
     cloudsync--test-s3-cloud-file
     "cloudsync/test-s3-remove-from-local: original content")))

(ert-deftest cloudsync/test-gdrive-remove-from-cloud ()
  "GDrive: remove content from cloud, sync to local"
  (let ((cloudsync-always-show-diff nil)
        (cloudsync-confirm-before-overwrite nil))
    (cloudsync--run-test
     "cloudsync/test-gdrive-remove-from-local: original content
remove this"
     "cloudsync/test-gdrive-remove-from-local: original content
remove this"
     'rclone
     "cloudsync/test-gdrive-remove-from-local: original content"
     cloudsync--test-gdrive-cloud-file
     "cloudsync/test-gdrive-remove-from-local: original content")))

(ert-deftest cloudsync/test-s3-remove-from-both-same-content ()
  "S3: remove content from cloud and local, update ancestor"
  (let ((cloudsync-always-show-diff nil)
        (cloudsync-confirm-before-overwrite nil))
    (cloudsync--run-test
     "cloudsync/test-s3-remove-from-both-same-content: original content"
     "cloudsync/test-s3-remove-from-both-same-content: original content
remove this"
     's3
     "cloudsync/test-s3-remove-from-both-same-content: original content"
     cloudsync--test-s3-cloud-file
     "cloudsync/test-s3-remove-from-both-same-content: original content")))

(ert-deftest cloudsync/test-gdrive-remove-from-both-same-content ()
  "GDrive: remove content from cloud and local, update ancestor"
  (let ((cloudsync-always-show-diff nil)
        (cloudsync-confirm-before-overwrite nil))
    (cloudsync--run-test
     "cloudsync/test-gdrive-remove-from-both-same-content: original content"
     "cloudsync/test-gdrive-remove-from-both-same-content: original content
remove this"
     'rclone
     "cloudsync/test-gdrive-remove-from-both-same-content: original content"
     cloudsync--test-gdrive-cloud-file
     "cloudsync/test-gdrive-remove-from-both-same-content: original content")))

(ert-deftest cloudsync/test-s3-change-from-local ()
  "S3: change content in local, sync to cloud"
  (let ((cloudsync-always-show-diff nil)
        (cloudsync-confirm-before-overwrite nil))
    (cloudsync--run-test
     "cloudsync/test-s3-change-from-local: modified content"
     "cloudsync/test-s3-change-from-local: original content"
     's3
     "cloudsync/test-s3-change-from-local: original content"
     cloudsync--test-s3-cloud-file
     "cloudsync/test-s3-change-from-local: modified content")))

(ert-deftest cloudsync/test-gdrive-change-from-local ()
  "GDrive: change content in local, sync to cloud"
  (let ((cloudsync-always-show-diff nil)
        (cloudsync-confirm-before-overwrite nil))
    (cloudsync--run-test
     "cloudsync/test-gdrive-change-from-local: modified content"
     "cloudsync/test-gdrive-change-from-local: original content"
     'rclone
     "cloudsync/test-gdrive-change-from-local: original content"
     cloudsync--test-gdrive-cloud-file
     "cloudsync/test-gdrive-change-from-local: modified content")))

(ert-deftest cloudsync/test-s3-change-from-cloud ()
  "S3: change content in cloud, sync to local"
  (let ((cloudsync-always-show-diff nil)
        (cloudsync-confirm-before-overwrite nil))
    (cloudsync--run-test
     "cloudsync/test-s3-change-from-local: original content"
     "cloudsync/test-s3-change-from-local: original content"
     's3
     "cloudsync/test-s3-change-from-local: modified content"
     cloudsync--test-s3-cloud-file
     "cloudsync/test-s3-change-from-local: modified content")))

(ert-deftest cloudsync/test-gdrive-change-from-cloud ()
  "GDrive: change content in cloud, sync to local"
  (let ((cloudsync-always-show-diff nil)
        (cloudsync-confirm-before-overwrite nil))
    (cloudsync--run-test
     "cloudsync/test-gdrive-change-from-cloud: original content"
     "cloudsync/test-gdrive-change-from-cloud: original content"
     'rclone
     "cloudsync/test-gdrive-change-from-cloud: modified content"
     cloudsync--test-gdrive-cloud-file
     "cloudsync/test-gdrive-change-from-cloud: modified content")))

(ert-deftest cloudsync/test-s3-change-from-both-same-content ()
  "S3: change content in cloud, sync to local"
  (let ((cloudsync-always-show-diff nil)
        (cloudsync-confirm-before-overwrite nil))
    (cloudsync--run-test
     "cloudsync/test-s3-change-from-both-same-content: modified content"
     "cloudsync/test-s3-change-from-both-same-content: original content"
     's3
     "cloudsync/test-s3-change-from-both-same-content: modified content"
     cloudsync--test-s3-cloud-file
     "cloudsync/test-s3-change-from-both-same-content: modified content")))

(ert-deftest cloudsync/test-gdrive-change-from-both-same-content ()
  "GDrive: change content in cloud, sync to local"
  (let ((cloudsync-always-show-diff nil)
        (cloudsync-confirm-before-overwrite nil))
    (cloudsync--run-test
     "cloudsync/test-gdrive-change-from-both-same-content: modified content"
     "cloudsync/test-gdrive-change-from-both-same-content: original content"
     'rclone
     "cloudsync/test-gdrive-change-from-both-same-content: modified content"
     cloudsync--test-gdrive-cloud-file
     "cloudsync/test-gdrive-change-from-both-same-content: modified content")))

;; TODO merge (with ancestor)
;; TODO no ancestor merge

;; negative tests

(ert-deftest cloudsync/unknown-cloud-service ()
  :expected-result :failed
  (cloudsync-sync (concat cloudsync--local-testdir "file.txt") 'fail "remotefile"))

(ert-deftest cloudsync/rclone-filenames-dont-match ()
  :expected-result :failed
  (cloudsync-sync (concat cloudsync--local-testdir "file.txt") 'rclone "goog:cloudsync/notfile.txt"))

;; helpers

(defun cloudsync--write-local (content fname)
  "Write CONTENT to local file FNAME"
  (with-temp-buffer
    (insert content)
    (write-region (point-min) (point-max) fname)))

(defun cloudsync--read-local (fname)
  "Read content in local file FNAME"
  (with-temp-buffer
    (insert-file-contents fname)
    (buffer-string)))

(defun cloudsync--run-test (orig-local-content orig-ancestor-content cloud-service
                                            orig-cloud-content cloud-file merged-content)
  "Set up for a test.  Put files in place.
ORIG-LOCAL-CONTENT is the content with local file content.
ORIG-ANCESTOR-CONTENT is the content with ancestor file content.
CLOUD-SERVICE is the cloud service to use
ORIG-CLOUD-CONTENT is the content with cloud file content
CLOUD-FILE is the filename to use for cloud file
MERGED-CONTENT is the content that should end up in local, ancestor and cloud"
  ;; stage or delete ancestor file
  (cond
   ;; file exists and we don't want it, delete it
   ((and (null orig-ancestor-content) (file-exists-p cloudsync--test-ancestor-file))
    (delete-file cloudsync--test-ancestor-file))
   ;; file doesn't exist and we don't want it, do nothing
   ((and (null orig-ancestor-content) (not (file-exists-p cloudsync--test-ancestor-file))))
   ;; we want the file, copy it
   (t
    (cloudsync--write-local orig-ancestor-content cloudsync--test-ancestor-file)
    (set-file-modes cloudsync--test-ancestor-file #o600)))

  ;; stage or delete cloud file. write to local temp file then push to
  ;; cloud
  (if (null orig-cloud-content)
      (cloudsync-delete cloud-service cloud-file t)
    (cloudsync--write-local  orig-cloud-content cloudsync--test-local-file)
    (cloudsync-push-overwrite cloudsync--test-local-file cloud-service cloud-file))

  ;; stage or delete local file, unless it's the same as the cloud
  ;; file since we already copied the cloud file to the local file's
  ;; path
  (cond
   ;; file exists and we don't want it, delete it
   ((and (null orig-local-content) (file-exists-p cloudsync--test-local-file))
    (delete-file cloudsync--test-local-file))
   ;; file doesn't exist and we don't want it, do nothing
   ((and (null orig-local-content) (not (file-exists-p cloudsync--test-local-file))))
   ;; we want the file and we didn't already copy it, copy it
   ((not (string= orig-local-content orig-cloud-content))
    (cloudsync--write-local orig-local-content cloudsync--test-local-file)))

  ;; run cloudsync
  (cloudsync-sync cloudsync--test-local-file cloud-service cloud-file)

  ;; check results, after sync local, ancestor and cloud should be identical
  (should (string=
           merged-content
           (cloudsync--read-local cloudsync--test-local-file)))
  (should (string=
           merged-content
           (cloudsync--read-local cloudsync--test-ancestor-file)))
  ;; overwrite local since rclone requires matching filename
  (cloudsync-fetch-overwrite cloudsync--test-local-file cloud-service cloud-file)
  (should (string=
           merged-content
           (cloudsync--read-local cloudsync--test-local-file))))

