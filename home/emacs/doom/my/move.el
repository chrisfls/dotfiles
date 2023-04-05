;;; -*- lexical-binding: t; -*-

(defun my/back-to-indentation-or-bol ()
  (interactive)
  (if (bolp)
      (back-to-indentation)
    (if (= (point) (progn (back-to-indentation) (point)))
        (beginning-of-line))))

(defun my/exchange-point-and-pairs ()
  (interactive)
  (let ((pos (point)))
    (sp-down-sexp)
    (sp-end-of-sexp)
    (when (= pos (point))
      (sp-beginning-of-sexp)
      (backward-char))))

(defun my/char-forward-step (char)
  (let ((pos (point)))
    (forward-char)
    (if (ignore-errors (search-forward char nil nil 1))
        (backward-char)
      (goto-char pos))))

(defun my/char-backward-step (char)
  (let ((pos (point)))
    (backward-char)
    (unless (ignore-errors (search-backward char nil nil 1))
      (goto-char pos))))

(defun my/backward-round ()
  (interactive)
  (my/char-backward-step "("))

(defun my/forward-round ()
  (interactive)
  (my/char-forward-step ")"))

(defun my/backward-curly ()
  (interactive)
  (my/char-backward-step "{"))

(defun my/forward-curly ()
  (interactive)
  (my/char-forward-step "}"))

(defun my/backward-square ()
  (interactive)
  (my/char-backward-step "["))

(defun my/forward-square ()
  (interactive)
  (my/char-forward-step "]"))

(defvar saved-char nil)

(defun my/char-forward ()
  (interactive)
  (setq saved-char (if (or (not ryo-modal-repeating) (null saved-char))
                       (read-char "Go to char: ")
                       saved-char))
  (my/char-forward-step (string saved-char)))

(defun my/char-backward ()
  (interactive)
  (setq saved-char (if (or (not ryo-modal-repeating) (null saved-char))
                       (read-char "Go to char (backward): ")
                       saved-char))
  (my/char-backward-step (string saved-char)))
