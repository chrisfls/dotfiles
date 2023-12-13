;;; -*- lexical-binding: t; -*-

(setq treemacs-git-mode 'deferred
      treemacs-width 35
      treemacs-recenter-after-file-follow t
      treemacs-recenter-after-tag-follow t)

(use-package! treemacs
  :after doom-themes
  :config
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-git-commit-diff-mode t)
  (treemacs-hide-gitignored-files-mode t)
  (treemacs-project-follow-mode t)
  ;; fix sidebar +- signs
  (unless (display-graphic-p)
    (setq doom-themes-treemacs-theme "Default")))
