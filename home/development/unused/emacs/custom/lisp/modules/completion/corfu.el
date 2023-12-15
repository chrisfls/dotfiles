;;; -*- lexical-binding: t; -*-

(use-package corfu
  :hook (after-init . global-corfu-mode))

(use-package corfu-prescient
  :after (:all corfu prescient)
  :hook (after-init . corfu-prescient-mode))
