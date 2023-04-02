(use-package! smartparens)

;;; movements

(defun back-to-indentation-or-beginning-of-line ()
  (interactive)
  (if (bolp)
      (back-to-indentation)
    (if (= (point) (progn (back-to-indentation) (point)))
        (beginning-of-line))))

(defun match-sexp ()
  (interactive)
  (let ((pos (point)))
    (sp-end-of-sexp)
    (when (= pos (point))
      (sp-beginning-of-sexp))))

;;; manipulations

(defun delete-forward-char-or-region (arg)
  (interactive "p")
  (if (use-region-p)
      (delete-region (region-beginning) (region-end))
    (delete-char arg)))

(defun delete-backward-char-or-kill-region (arg)
  (interactive "p")
  (if (use-region-p)
      (kill-region (region-beginning) (region-end))
    (delete-backward-char arg)))

;;; search

(defun isearch-repeat-forward-regexp ()
  (interactive)
  (setq ryo-modal--last-command-prefix-arg nil)
  (setq ryo-modal--last-command #'isearch-forward-regexp)
  (if isearch-regexp (isearch-repeat-forward) (isearch-forward-regexp)))

(defun isearch-repeat-backward-regexp ()
  (interactive)
  (setq ryo-modal--last-command-prefix-arg nil)
  (setq ryo-modal--last-command #'isearch-backward-regexp)
  (if isearch-regexp (isearch-repeat-backward) (isearch-backward-regexp)))

(provide 'smarter-commands)
