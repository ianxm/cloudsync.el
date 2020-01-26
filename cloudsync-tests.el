(require 'cloudsync)
(require 'cloudsync-testparams)

;; stage test data
(progn
  (unless (file-exists-p cloudsync-ancestor-dir)
    (make-directory cloudsync-ancestor-dir)
    (set-file-modes cloudsync-ancestor-dir #o700))

  (copy-file
   (concat cloudsync--local-testdir "ancestor.txt")
   (concat cloudsync-ancestor-dir "local_changed.txt") t)
  (set-file-modes (concat cloudsync-ancestor-dir "local_changed.txt") #o600)
  (message "wrote %s" (concat cloudsync-ancestor-dir "local_changed.txt"))

  (copy-file
   (concat cloudsync--local-testdir "local_changed_orig.txt")
   (concat cloudsync--local-testdir "local_changed.txt") t)
  (message "wrote %s" (concat cloudsync--local-testdir "local_changed.txt"))

  (copy-file
   (concat cloudsync--local-testdir "local_changed_orig.txt")
   (concat cloudsync--local-testdir "local_no_ancestor.txt") t)
  (message "wrote %s" (concat cloudsync--local-testdir "local_no_ancestor.txt"))

  (copy-file
   (concat cloudsync--local-testdir "ancestor.txt")
   (concat cloudsync-ancestor-dir "local_one_change.txt") t)
  (set-file-modes (concat cloudsync-ancestor-dir "local_one_change.txt") #o600)
  (message "wrote %s" (concat cloudsync-ancestor-dir "local_one_change.txt"))

  (copy-file
   (concat cloudsync--local-testdir "local_one_change_orig.txt")
   (concat cloudsync--local-testdir "local_one_change.txt") t)
  (message "wrote %s" (concat cloudsync--local-testdir "local_one_changed.txt"))

  (copy-file
   (concat cloudsync--local-testdir "ancestor.txt")
   (concat cloudsync-ancestor-dir "local_no_diff.txt") t)
  (set-file-modes (concat cloudsync-ancestor-dir "local_no_diff.txt") #o600)
  (message "wrote %s" (concat cloudsync-ancestor-dir "local_no_diff.txt"))

  (when (file-exists-p (concat cloudsync-ancestor-dir "local_no_ancestor.txt"))
    (delete-file (concat cloudsync-ancestor-dir "local_no_ancestor.txt"))
    (message "deleted %s" (concat cloudsync-ancestor-dir "local_no_ancestor.txt")))

  (cloudsync-push-overwrite (concat cloudsync--local-testdir "remote.txt")
                            's3
                            (concat cloudsync--remote-testdir "remote.txt"))
  (message "pushed %s" (concat cloudsync--remote-testdir "remote.txt"))

  (cloudsync-push-overwrite (concat cloudsync--local-testdir "remote_conflict.txt")
                            's3
                            (concat cloudsync--remote-testdir "remote_conflict.txt"))
  (message "pushed %s" (concat cloudsync--remote-testdir "remote_conflict.txt"))

  "init done")

;; -- push
(cloudsync-push-overwrite (concat cloudsync--local-testdir "remote.txt")
                          's3
                          (concat cloudsync--remote-testdir "remote.txt"))

(cloudsync-push-overwrite (concat cloudsync--local-testdir "remote_conflict.txt")
                          's3
                          (concat cloudsync--remote-testdir "remote_conflict.txt"))

;; -- fetch
(cloudsync-fetch-overwrite (concat cloudsync--local-testdir "remote_copy.txt"))

(cloudsync-fetch-overwrite (concat cloudsync--local-testdir "remote_copy.txt")
                           's3
                           (concat cloudsync--remote-testdir "remote.txt"))

(cloudsync-fetch-overwrite (concat cloudsync--local-testdir "remote_copy.txt")
                           's3
                           (concat cloudsync--remote-testdir "remote_conflict.txt"))


;; -- sync

;; failure: cloud file doesn't exist
(cloudsync-sync (concat cloudsync--local-testdir "local_no_diff.txt")
                's3
                (concat cloudsync--remote-testdir "remote_doesnt_exist.txt"))

;; no diff
(cloudsync-sync (concat cloudsync--local-testdir "local_no_diff.txt")
                's3
                (concat cloudsync--remote-testdir "remote.txt"))

;; no remote changes, just push local
(cloudsync-sync (concat cloudsync--local-testdir "local_changed.txt")
                's3
                (concat cloudsync--remote-testdir "remote.txt"))

;; fast forward (both changed but no conflict; needs ancestor)
(cloudsync-sync (concat cloudsync--local-testdir "local_one_change.txt")
                's3
                (concat cloudsync--remote-testdir "remote_conflict.txt"))

;; merge (needs ancestor)
(cloudsync-sync (concat cloudsync--local-testdir "local_changed.txt")
                's3
                (concat cloudsync--remote-testdir "remote_conflict.txt"))

;; no ancestor merge
(cloudsync-sync (concat cloudsync--local-testdir "local_no_ancestor.txt")
                's3
                (concat cloudsync--remote-testdir "remote.txt"))
