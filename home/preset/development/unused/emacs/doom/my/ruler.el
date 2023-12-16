;;; -*- lexical-binding: t; -*-

(defun my/ruler (col)
  (setq display-fill-column-indicator-column col)
  (display-fill-column-indicator-mode t))
