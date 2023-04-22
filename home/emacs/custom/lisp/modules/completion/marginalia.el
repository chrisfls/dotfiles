;;; -*- lexical-binding: t; -*-

(use-package marginalia
  :autoload marginalia-mode
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))
