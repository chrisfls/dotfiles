(use-package! repeat-nth
  :demand t)

(defvar saved-char nil)

;;; TODO: maybe error when character is not found

(defun go-to-char-forward ()
  (interactive)
  (setq saved-char (if (or (not ryo-modal-repeating) (null saved-char))
                       (read-char "Go to char: ")
                       saved-char))
  (setq ryo-modal--last-command-prefix-arg nil)
  (setq ryo-modal--last-command #'go-to-char-forward)
  (let ((pos (point)))
    (forward-char)
    (unless (ignore-errors (search-forward (string saved-char) nil nil 1))
      (goto-char pos))))

(defun go-to-char-backward ()
  (interactive)
  (setq saved-char (if (or (not ryo-modal-repeating) (null saved-char))
                       (read-char "Go to char (backward): ")
                       saved-char))
  (setq ryo-modal--last-command-prefix-arg nil)
  (setq ryo-modal--last-command #'go-to-char-backward)
  (let ((pos (point)))
    (backward-char)
    (if (ignore-errors (search-backward (string saved-char) nil nil 1))
      (forward-char)
      (goto-char pos))))

(defun go-till-char-forward ()
  (interactive)
  (setq saved-char (if (or (not ryo-modal-repeating) (null saved-char))
                       (read-char "Go till char: ")
                       saved-char))
  (setq ryo-modal--last-command-prefix-arg nil)
  (setq ryo-modal--last-command #'go-till-char-forward)
  (let ((pos (point)))
    (forward-char)
    (if (ignore-errors (search-forward (string saved-char) nil nil 1))
      (backward-char)
      (goto-char pos))))

(defun go-till-char-backward ()
  (interactive)
  (setq saved-char (if (or (not ryo-modal-repeating) (null saved-char))
                       (read-char "Go till char (backward): ")
                       saved-char))
  (setq ryo-modal--last-command-prefix-arg nil)
  (setq ryo-modal--last-command #'go-till-char-backward)
  (let ((pos (point)))
    (backward-char)
    (unless (ignore-errors (search-backward (string saved-char) nil nil 1))
      (goto-char pos))))

(provide 'char-occurrence)
