;;; -*- lexical-binding: t; -*-

(load! "../evil-core")

(evil-esc-mode 1)

;;; point-undo

(use-package! point-undo
  :demand t
  :init)

;;; which-key

(use-package! which-key
  :init
  (which-key-mode))

(load! "../commands/grab")
(load! "../commands/macro")
(load! "../commands/search")
(load! "wakib/wakib-keys")

;;; wakib-keys

(define-key input-decode-map [?\C-i] [C-i])
(define-key input-decode-map [?\C-m] [C-m])
(define-key input-decode-map [?\C-\S-m] [C-S-m])

(defun my/kill-region-or-whole-line (arg)
  (interactive "p")
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (kill-whole-line arg)))

(wakib-keys 1)
