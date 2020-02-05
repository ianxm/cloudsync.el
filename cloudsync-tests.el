(require 'cloudsync)
(require 'cloudsync-testparams)
(require 'cloudsync-testhelpers)

;; failure: unknown cloud service
(cloudsync-sync (concat cloudsync--local-testdir "original.txt") 'fail "remotefile")

;; failure: local-file not found
(cloudsync-sync "/some/local/file1.txt" 'rclone "remote:dir/file2.txt")

;; failure: rclone filenames don't matche
(cloudsync-sync (concat cloudsync--local-testdir "original.txt") 'rclone "remote:dir/original2.txt")

;; files match, do nothing
(cloudsync--test-no-diff 's3 cloudsync--test-s3-cloud-file)
(cloudsync--test-no-diff 'rclone cloudsync--test-goog-cloud-file)

;; no remote changes, push local changes
(cloudsync--test-no-remote-changes 's3 cloudsync--test-s3-cloud-file)
(cloudsync--test-no-remote-changes 'rclone cloudsync--test-goog-cloud-file)

;; no remote file, push local file
(cloudsync--test-no-remote-file 's3 cloudsync--test-s3-cloud-file)
(cloudsync--test-no-remote-file 'rclone cloudsync--test-goog-cloud-file)

;; no local changes, pull remote changes
(cloudsync--test-no-local-changes 's3 cloudsync--test-s3-cloud-file)
(cloudsync--test-no-local-changes 'rclone cloudsync--test-goog-cloud-file)

;; no local file, pull remote file
(cloudsync--test-no-local-file 's3 cloudsync--test-s3-cloud-file)
(cloudsync--test-no-local-file 'rclone cloudsync--test-goog-cloud-file)

;; both changed but no conflicts, merge and update both
(cloudsync--test-fast-forward 's3 cloudsync--test-s3-cloud-file)
(cloudsync--test-fast-forward 'rclone cloudsync--test-goog-cloud-file)

;; both changed, resolve conflicts with ediff and update both
(cloudsync--test-merge 's3 cloudsync--test-s3-cloud-file)
(cloudsync--test-merge 'rclone cloudsync--test-goog-cloud-file)

;; both changed, and there's no ancestor file, resolve conflicts with ediff and update both
(cloudsync--test-no-ancestor-merge 's3 cloudsync--test-s3-cloud-file)
(cloudsync--test-no-ancestor-merge 'rclone cloudsync--test-goog-cloud-file)
