;; -*- lexical-binding: t; -*-

(setq scale (string-to-number (getenv "GDK_DPI_SCALE")))

(setq use-package-always-demand (string= (getenv "EMACS_DUMP") "yes"))

;;;;;;;;;;;;;;;;;;;;
;;; useful hooks ;;;
;;;;;;;;;;;;;;;;;;;;

(defvar after-enable-theme-hook nil
   "Normal hook run after enabling a theme.")

(defun run-after-enable-theme-hook (&rest _args)
   "Run `after-enable-theme-hook'."
   (run-hooks 'after-enable-theme-hook))

(advice-add 'enable-theme :after #'run-after-enable-theme-hook)

;;;;;;;;;;;;;;;;;;;;
;;; load modules ;;;
;;;;;;;;;;;;;;;;;;;;

;; (defun my-load-all-in-directory (dir)
;;   "`load' all elisp libraries in directory DIR which are not already loaded."
;;   (interactive "D")
;;   (let ((libraries-loaded (mapcar #'file-name-sans-extension
;;                                   (delq nil (mapcar #'car load-history)))))
;;     (dolist (file (directory-files dir t ".+\\.elc?$"))
;;       (let ((library (file-name-sans-extension file)))
;;         (unless (member library libraries-loaded)
;;           (load library nil t)
;;           (push library libraries-loaded))))))

;; load all from lisp/**/*.el
;; (my-load-all-in-directory (expand-file-name "lisp" user-emacs-directory))

(require 'find-lisp)

(mapcar
  (lambda (fn)
    (load (file-name-sans-extension fn)))
  (find-lisp-find-files
    (expand-file-name "lisp" user-emacs-directory)
    "\\.el\\'"))

;; install packages asynchronously
(if use-package-always-demand
    (elpaca-wait)
    (elpaca-process-queues))
