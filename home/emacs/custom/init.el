;;; -*- lexical-binding: t; -*-

;;; run init hooks early when building
(if my/sync-flag
    (run-hooks 'my-before-init-hook)
  (setq debug-on-error 'my-before-init-hook))

;;; add the modules folder to the load-path
(setq user-init-file (or load-file-name (buffer-file-name)))
(add-to-list 'load-path (expand-file-name "lisp" (file-name-directory user-init-file)))

(load "straight")
(load "settings")

(if my/sync-flag
    (progn
      (straight-prune-build)
      (straight-freeze-versions)
      (message "Emacs synced in %s."
        (format "%.2f seconds"
          (float-time
            (time-subtract after-init-time before-init-time)))))
  (message "Emacs loaded in %s with %d garbage collections."
    (format "%.2f seconds"
      (float-time
        (time-subtract after-init-time before-init-time)))
    gcs-done))
