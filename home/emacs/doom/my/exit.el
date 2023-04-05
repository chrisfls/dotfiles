;;; -*- lexical-binding: t; -*-

(defun my/end-of-region ()
  (interactive)
  (when (region-active-p)
    (when (< (point) (mark))
      (exchange-point-and-mark))
    (deactivate-mark)))

(defun my/beginning-of-region ()
  (interactive)
  (when (region-active-p)
    (when (> (point) (mark))
      (exchange-point-and-mark))
    (deactivate-mark)))

(defun my/open-line-down ()
  (interactive)
  (end-of-line)
  (sp-newline))

(defun my/open-line-up ()
  (interactive)
  (previous-line)
  (end-of-line)
  (sp-newline))
