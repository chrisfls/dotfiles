;;; -*- lexical-binding: t; -*-

(defun my/expand-region ()
  (interactive)
  (er/expand-region 1)
  (exchange-point-and-mark))

(defun my/contract-region ()
  (interactive)
  (er/contract-region 1)
  (when (and (region-active-p) (< (point) (mark)))
    (exchange-point-and-mark)))

(defun my/deactivate-mark-command ()
  (interactive)
  (deactivate-mark))

(defun my/set-mark-if-inactive ()
  (interactive)
  (unless mark-active (set-mark (point))))
  
(defun my/deactivate-mark-if-active ()
  (interactive)
  (if mark-active (deactivate-mark)))

(defun my/mark-word ()
  (interactive)
  (er/mark-word)
  (exchange-point-and-mark))

(defun my/mark-symbol ()
  (interactive)
  (er/mark-symbol)
  (exchange-point-and-mark))

(defun my/mark-sentence ()
  (interactive)
  (er/mark-sentence)
  (exchange-point-and-mark))

(defun my/mark-paragraph ()
  (interactive)
  (er/mark-text-paragraph)
  (exchange-point-and-mark))

(defun my/mark-whole-buffer ()
  (interactive)
  (beginning-of-buffer)
  (mark-whole-buffer)
  (exchange-point-and-mark))

;;; quotes

(defun my/mark-inside-quotes ()
  (interactive)
  (er/mark-inside-quotes)
  (exchange-point-and-mark))

(defun my/mark-outside-quotes ()
  (interactive)
  (er/mark-outside-quotes)
  (exchange-point-and-mark))

;;; defun

(defun my/mark-inside-defun ()
  (interactive)
  (er/mark-defun)
  (when (region-active-p)
    (sp-down-sexp)
    (deactivate-mark)
    (er/mark-inside-pairs)
    (exchange-point-and-mark)))

(defun my/mark-outside-defun ()
  (interactive)
  (er/mark-defun)
  (exchange-point-and-mark))

(defun my/sexp-up-to (delim &optional start-point)
  (setq start-point (or start-point (point)))
  (let ((pos (point)))
    (sp-backward-up-sexp)
    (if (/= pos (point))
        (or (= (char-after) delim)
            (my/sexp-up-to delim start-point))
      (goto-char start-point)
      nil)))

;; round

(defun my/mark-inside-round ()
  (interactive)
  (when (my/sexp-up-to (string-to-char "("))
    (sp-down-sexp)
    (er/mark-inside-pairs)
    (exchange-point-and-mark)))

(defun my/mark-outside-round ()
  (interactive)
  (when (my/sexp-up-to (string-to-char "("))
    (sp-down-sexp)
    (er/mark-outside-pairs)
    (exchange-point-and-mark)))
  
;;; curly

(defun my/mark-inside-curly ()
  (interactive)
  (when (my/sexp-up-to (string-to-char "{"))
    (sp-down-sexp)
    (er/mark-inside-pairs)
    (exchange-point-and-mark)))

(defun my/mark-outside-curly ()
  (interactive)
  (when (my/sexp-up-to (string-to-char "{"))
    (sp-down-sexp)
    (er/mark-outside-pairs)
    (exchange-point-and-mark)))

;;; square

(defun my/mark-inside-square ()
  (interactive)
  (when (my/sexp-up-to (string-to-char "["))
    (sp-down-sexp)
    (er/mark-inside-pairs)
    (exchange-point-and-mark)))

(defun my/mark-outside-square ()
  (interactive)
  (when (my/sexp-up-to (string-to-char "["))
    (sp-down-sexp)
    (er/mark-outside-pairs)
    (exchange-point-and-mark)))

(defun my/mark-outside-square ()
  (interactive)
  (when (my/sexp-up-to (string-to-char "["))
    (sp-down-sexp)
    (er/mark-outside-pairs)
    (exchange-point-and-mark)))

;; lines

(defun my/mark-line-forward ()
  (interactive)
  (unless (region-active-p)
    (set-mark (line-beginning-position)))
  (when (< (point) (mark))
    (exchange-point-and-mark))
  (next-line)
  (beginning-of-line))

(defun my/mark-line-backward ()
  (interactive)
  (unless (region-active-p)
    (set-mark (line-end-position)))
  (when (> (point) (mark))
    (exchange-point-and-mark))
  (previous-line)
  (beginning-of-line))
