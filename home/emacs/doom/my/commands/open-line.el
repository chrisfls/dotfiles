(use-package! smartparens)

(defun open-line-down ()
  (interactive)
  (end-of-line)
  (sp-newline))

(defun open-line-up ()
  (interactive)
  (previous-line)
  (end-of-line)
  (sp-newline))

(provide 'open-line)
