;; -*- lexical-binding: t; -*-

;; color scheme
(use-package color-theme-sanityinc-tomorrow
  ;; TODO: change theme
  :init
  (load-theme 'sanityinc-tomorrow-bright t))

;; distinguish file buffers by background color
(use-package solaire-mode
  ;; TODO: configure
  :init
  (solaire-global-mode +1))

;; flash cursor after big movements
(use-package nav-flash
  ;; TODO: configure better
  :init
  (add-hook 'imenu-after-jump-hook 'nav-flash-show nil t))

;; TODO: configure
(use-package highlight-indent-guides)

;; TODO: configure
(use-package hl-todo)

;; TODO: configure
(use-package volatile-highlights)

;; TODO: configure
(use-package vi-tilde-fringe)

;; TODO: configure
(use-package git-gutter-fringe)

;; TODO: configure only for github emojis
(use-package emojify)


(use-package nerd-icons)

(use-package nerd-icons-completion)

