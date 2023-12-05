;;; -*- lexical-binding: t; -*-

;;; bootstrap straight
(eval-when-compile
  (defvar bootstrap-version)
  (let ((bootstrap-file
        (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
        (bootstrap-version 6))
    (unless (file-exists-p bootstrap-file)
      (with-current-buffer
          (url-retrieve-synchronously
          "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
          'silent 'inhibit-cookies)
        (goto-char (point-max))
        (eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage))
  (straight-use-package 'use-package))

(setq straight-enable-package-integration nil)
(setq straight-recipes-emacsmirror-use-mirror t)
(setq straight-use-package-by-default t)
(setq straight-cache-autoloads t)

(setq straight-check-for-modifications (if my/sync-flag
                                           'at-startup
                                           'never))
(setq use-package-always-defer (not my/sync-flag))
(setq use-package-always-demand my/sync-flag)

(use-package async
  :demand my/sync-flag
  :config
  (setq async-bytecomp-allowed-packages '(all))
  (async-bytecomp-package-mode 1))
