;;; -*- lexical-binding: t; -*-

;; (setq initial-frame-alist '((background-color . "black")))

(setq package-native-compile t)

(defvar my/sync-flag (member "--sync" command-line-args))

(defun my/sync-switch (switch)
  (setq my/sync-flag t))

(add-to-list 'command-switch-alist '("--sync" . my/sync-switch))

(when my/sync-flag
  (message "Syncing..."))

(when (boundp 'w32-pipe-read-delay)
  (setq w32-pipe-read-delay 0))

;;; Disable gc until enabling `gcmh-mode`.
(setq gc-cons-threshold most-positive-fixnum)

(setq package-enable-at-startup nil)

(setq inhibit-compacting-font-caches t)

;;; Skip checking mtime on elisp bytecode.
(setq load-prefer-newer my/sync-flag)

;;; Enable debug mode if requested.
(when (getenv-internal "DEBUG")
  (setq init-file-debug t
        debug-on-error t))

;;; Copied from github.com/doomemacs/doomemacs/blob/master/lisp/doom.el#L321-L351
(let ((old-value (default-toplevel-value 'file-name-handler-alist)))
  (setq file-name-handler-alist
        (if (eval-when-compile
              (locate-file-internal "calc-loaddefs.el" load-path))
            nil
          (list (rassq 'jka-compr-handler old-value))))
  (set-default-toplevel-value 'file-name-handler-alist file-name-handler-alist)
  (put 'file-name-handler-alist 'initial-value old-value)
  (add-hook 'emacs-startup-hook
    (defun my--reset-file-handler-alist-h ()
      (setq file-name-handler-alist
            (delete-dups (append file-name-handler-alist old-value))))
    101))

;;; Warn about version change.
(let ((old-version (eval-when-compile emacs-version)))
  (unless (equal emacs-version old-version)
    (user-error (concat "Packages were compiled with Emacs %s, but was loaded with %s.")
                emacs-version old-version)))

;;; Frame resizing during initialization yields ~1s lag.
(setq frame-inhibit-implied-resize t)

;;; Reduce *Message* noise at startup.
(setq inhibit-startup-screen t
      inhibit-startup-echo-area-message user-login-name)

;;; Remove "For information about GNU Emacs..." message at startup.
(advice-add #'display-startup-echo-area-message :override #'ignore)

;;; Suppress the vanilla startup screen completely. Setting `inhibit-startup-screen'
;;; disables it but it would still initialize anyway.
(advice-add #'display-startup-screen :override #'ignore)

;;; Other modes are a lot slower for startup.
(setq initial-major-mode 'fundamental-mode
      initial-scratch-message nil)

(defcustom my-before-init-hook ()
  "Runned after unsetting optimizations, replaces before-init-hook for unsupported packages."
  :group 'my
  :type 'hook)

;; Disable bars
(unless (memq window-system '(mac ns))
  (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(when (fboundp 'horizontal-scroll-bar-mode)
  (horizontal-scroll-bar-mode -1))

;;; Copied from github.com/doomemacs/doomemacs/blob/master/lisp/doom.el
(unless init-file-debug
  (define-advice load-file (:override (file) silence)
    (load file nil 'nomessage))
  (define-advice startup--load-user-init-file (:before (&rest _) undo-silence)
    (advice-remove #'load-file #'load-file@silence))
  (put 'load-suffixes 'initial-value (default-toplevel-value 'load-suffixes))
  (put 'load-file-rep-suffixes 'initial-value (default-toplevel-value 'load-file-rep-suffixes))
  (set-default-toplevel-value 'load-suffixes '(".elc" ".el"))
  (set-default-toplevel-value 'load-file-rep-suffixes '(""))
  (add-hook 'my-before-init-hook
    (defun my--reset-load-suffixes-h ()
      (setq load-suffixes (get 'load-suffixes 'initial-value)
            load-file-rep-suffixes (get 'load-file-rep-suffixes 'initial-value))))
  (setq custom-dont-initialize t)
  (add-hook 'my-before-init-hook
    (defun my--reset-custom-dont-initialize-h ()
      (setq custom-dont-initialize nil)))
  (setq-default inhibit-redisplay t
                inhibit-message t)
  (defun my--reset-inhibited-vars-h ()
    (setq-default inhibit-redisplay nil
                  inhibit-message nil)
    (redraw-frame))
  (add-hook 'after-init-hook #'my--reset-inhibited-vars-h)
  (define-advice startup--load-user-init-file (:after (&rest _) undo-inhibit-vars)
    (my--reset-inhibited-vars-h)
    (unless (default-toplevel-value 'mode-line-format)
      (setq-default mode-line-format (get 'mode-line-format 'initial-value))))
  (advice-add #'tool-bar-setup :override #'ignore)
  (defun my--tool-bar-setup ()
    (tool-bar-setup)
    (remove-hook 'tool-bar-mode #'my--tool-bar-setup))
  (define-advice startup--load-user-init-file (:before (&rest _) defer-tool-bar-setup)
    (advice-remove #'tool-bar-setup #'ignore)
    (add-hook 'tool-bar-mode #'my--tool-bar-setup))
  (setq command-line-ns-option-alist nil))
