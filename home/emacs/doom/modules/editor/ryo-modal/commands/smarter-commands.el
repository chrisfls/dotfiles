(use-package! smartparens)

;;; movements

(defun back-to-indentation-or-beginning-of-line ()
  (interactive)
  (if (bolp)
      (back-to-indentation)
    (if (= (point) (progn (back-to-indentation) (point)))
        (beginning-of-line))))

(defun match-sexp (&optional arg)
  (interactive)
  (let ((pos (point)))
    (sp-end-of-sexp)
    (when (= pos (point))
      (sp-beginning-of-sexp))))

;;; manipulations

(defun delete-forward-char-or-region (&optional arg)
  (interactive)
  (if (use-region-p)
      (delete-region (region-beginning) (region-end))
    (delete-char arg)))

(defun delete-backward-char-or-kill-region (&optional arg)
  (interactive)
  (if (use-region-p)
      (kill-region (region-beginning) (region-end))
    (delete-backward-char arg)))

(provide 'smarter-commands)
