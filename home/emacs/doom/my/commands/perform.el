;;; -*- lexical-binding: t; -*-

(defun my/kill-region-or-backward-char (arg)
  (interactive "p")
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (delete-backward-char arg)))
