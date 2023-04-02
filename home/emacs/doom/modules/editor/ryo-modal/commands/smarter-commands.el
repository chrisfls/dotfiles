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

(defun isearch-region-forward ()
  (interactive)
  (setq ryo-modal--last-command-prefix-arg nil)
  (setq ryo-modal--last-command #'isearch-repeat-forward)
  (if (use-region-p)
      (progn
        (setq isearch-forward t)
        (setq isearch-regexp nil)
        (setq isearch-string (buffer-substring (region-beginning) (region-end)))
        (deactivate-mark))
    (isearch-forward-regexp)))

(defun isearch-region-backward ()
  (interactive)
  (setq ryo-modal--last-command-prefix-arg nil)
  (setq ryo-modal--last-command #'isearch-repeat-backward)
  (if (use-region-p)
      (progn
        (setq isearch-forward nil)
        (setq isearch-regexp nil)
        (setq isearch-string (buffer-substring (region-beginning) (region-end)))
        (deactivate-mark))
    (isearch-backward-regexp)))

(defun isearch-repeat-forward-region ()
  (interactive)
  (setq ryo-modal--last-command-prefix-arg nil)
  (setq ryo-modal--last-command #'isearch-repeat-forward)
  (if (or isearch-regexp isearch-string)
      (isearch-repeat-forward)
    (if (use-region-p)
        (progn
          (setq isearch-forward t)
          (setq isearch-regexp nil)
          (setq isearch-string (buffer-substring (region-beginning) (region-end)))
          (deactivate-mark))
      (isearch-forward-regexp))))

(defun isearch-repeat-backward-region ()
  (interactive)
  (setq ryo-modal--last-command-prefix-arg nil)
  (setq ryo-modal--last-command #'isearch-repeat-backward)
  (if (or isearch-regexp isearch-string)
      (isearch-repeat-backward)
    (if (use-region-p)
        (progn
          (setq isearch-forward nil)
          (setq isearch-regexp nil)
          (setq isearch-string (buffer-substring (region-beginning) (region-end)))
          (deactivate-mark))
      (isearch-backward-regexp))))

(provide 'smarter-commands)
