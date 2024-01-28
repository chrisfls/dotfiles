(defun my/kill-region-or-line ()
  (interactive)
  (setq unread-command-events
        (if (use-region-p)
            (listify-key-sequence "\C-w")
          (listify-key-sequence "\C-k"))))

(defun my/mark ()
  (interactive)
  (unless (region-active-p)
    (push-mark (point) nil t))
  (activate-mark))

(defun my/comment ()
  (interactive)
  (if (use-region-p)
      (comment-or-uncomment-region (region-beginning) (region-end))
    (comment-or-uncomment-region (line-beginning-position) (line-end-position)))
  (setq deactivate-mark nil))

(use-package! ryo-modal
  :commands ryo-modal-mode
  :hook (doom-after-modules-config . ryo-modal-mode)
  :config
  (setq ryo-modal-default-cursor-color "#ffffff") ;; todo: fix
  (ryo-modal-keys
    (:norepeat t)
    ("0" "M-0")
    ("1" "M-1")
    ("2" "M-2")
    ("3" "M-3")
    ("4" "M-4")
    ("5" "M-5")
    ("6" "M-6")
    ("7" "M-7")
    ("8" "M-8")
    ("9" "M-9"))
  (ryo-modal-keys
    ;; char/line
    ("h" "C-b") ; backward-char
    ("H" "C-b" :first '(my/mark))
    ("j" "C-n")  ; next-line
    ("J" "C-n" :first '(my/mark))
    ("k" "C-p") ; previous-line
    ("K" "C-p" :first '(my/mark))
    ("l" "C-f") ; forward-char
    ("L" "C-f" :first '(my/mark))

    ;; word
    ("w" "M-b") ; backward-word
    ("W" "M-b" :first '(my/mark))
    ("e" "M-f") ; forward-word
    ("E" "M-f" :first '(my/mark))

    ;; paragraph
    ("[" "M-{") ; backward-paragraph
    ("{" "M-{" :first '(my/mark))
    ("]" "M-}") ; forward-paragraph
    ("}" "M-}" :first '(my/mark))

    ;; undo/redo
    ("u" "C-_") ; undo-tree-undo
    ("U" "C-M-_") ; undo-tree-redo

    ;; cut/copy/paste
    ("d" my/kill-region-or-line)
    ("y" "M-w") ; copy
    ("p" "C-y") ; yank

    ;; delete characters
    ("x" "<deletechar>")
    ("X" "DEL")

    ;; exits
    ;("a" +default/newline-above :exit t)
    ;("O" +default/newline-below :exit t)

    ("v" "C-SPC")
    (";" exchange-point-and-mark)
    ("'" ryo-modal-repeat)
    ("/" my/comment)))

(global-set-key (kbd "<escape>") 'ryo-modal-mode)

;; '("m" . meow-join)
;; '("n" . meow-search)
;; '("S" . meow-goto-line) ; Q/X
;; '("P" . meow-replace) ; r
;; '("t" . meow-till)
;; '("v" . meow-visit)
;; '("s" . meow-line) ; x
;; '("f" . meow-find)
