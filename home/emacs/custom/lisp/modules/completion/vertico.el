;;; -*- lexical-binding: t; -*-

(use-package vertico
  :straight (vertico :files (:defaults "extensions/*")
                     :includes (vertico-multiform))
  :hook (after-init . vertico-mode))

(use-package vertico-posframe
  :after (:all vertico)
  :hook (after-init . vertico-posframe-mode)
  :config
  (setq vertico-posframe-poshandler #'posframe-poshandler-frame-top-center))

(use-package vertico-prescient
  :after (:all vertico prescient)
  :hook (after-init . vertico-prescient-mode))
