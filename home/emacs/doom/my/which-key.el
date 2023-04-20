;;; -*- lexical-binding: t; -*-

(use-package! which-key
  :config
  (setq which-key-show-early-on-C-h t
        which-key-idle-delay 0.05
        which-key-idle-secondary-delay 0.05
        which-key-popup-type 'side-window
        which-key-side-window-location '(right bottom)))
