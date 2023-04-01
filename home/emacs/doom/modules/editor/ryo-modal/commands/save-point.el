(defvar saved-point nil)

(defun save-point ()
  (interactive)
  (setq saved-point (point))
  (message "Point saved."))

(defun restore-point ()
  (interactive)
  (let ((pos saved-point))
    (if pos
        (progn
          (setq saved-point (point))
          (goto-char pos)
          (message "Point restored."))
      (message "No point has been saved yet."))))

(provide 'save-point)
