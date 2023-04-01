
(defun go-to-char-forward (&optional arg char)
  (interactive "p\ncGo to char (forward): ")
  (setq arg (or arg 1))
  (let ((pos (point)))
    (forward-char arg)
    (unless (or (if (> arg 0)
                    (search-forward (string char) nil nil 1)
                    (search-backward (string char) nil nil 1))
                (eq (char-after) char))
      (goto-char pos))))

(defun go-to-char-backward (&optional arg char)
  (interactive "p\ncGo to char (backward): ")
  (go-to-char-forward (- arg) char))

(defun go-till-char-forward (&optional arg char)
  (interactive "p\ncGo till char (forward): ")
  (let ((pos (point)))
    (go-to-char-forward arg char)
    (when (/= pos (point))
      (backward-char))))

(defun go-till-char-backward (&optional arg char)
  (interactive "p\ncGo till char (backward): ")
  (go-till-char-forward (- arg) char))

(provide 'char-occurrence)
