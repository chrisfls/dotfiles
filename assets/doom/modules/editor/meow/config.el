;;; editor/meow/config.el -*- lexical-binding: t; -*-

(setq doom-leader-key "C-c"
      doom-leader-alt-key "C-c"
      doom-localleader-key "C-c l"
      doom-localleader-alt-key "C-c l")

(use-package! meow
  :hook (doom-after-modules-config . meow-global-mode)
  :demand t
  :config
  (meow-thing-register 'whitespace
                    '(syntax . " ")
                    '(syntax . " "))
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty
        meow-cursor-type-insert '(bar . 4)
        meow-cursor-type-region-cursor '(bar . 4)
        meow-cursor-type-keypad 'hollow
        meow-use-clipboard t
        meow-keypad-leader-dispatch doom-leader-map
        meow-keypad-start-keys '((?k . ?c)
                                 (?h . ?h)
                                 (?x . ?x))
        meow-char-thing-table '((?r . round)
                                (?s . square)
                                (?c . curly)
                                (?g . string)
                                (?e . symbol)
                                (?w . window)
                                (?b . buffer)
                                (?p . paragraph)
                                (?l . line)
                                (?d . defun)
                                (?. . sentence)
                                (?\s . whitespace))
        meow-use-enhanced-selection-effect t)
  (meow-motion-overwrite-define-key
   '("j" . meow-next)
   '("k" . meow-prev)
   '("<escape>" . ignore))
  (meow-leader-define-key
   ;; SPC j/k will run the original command in MOTION state.
   '("j" . "H-j")
   '("k" . "H-k")
   ;; Use SPC (0-9) for digit arguments.
   '("1" . meow-digit-argument)
   '("2" . meow-digit-argument)
   '("3" . meow-digit-argument)
   '("4" . meow-digit-argument)
   '("5" . meow-digit-argument)
   '("6" . meow-digit-argument)
   '("7" . meow-digit-argument)
   '("8" . meow-digit-argument)
   '("9" . meow-digit-argument)
   '("0" . meow-digit-argument)
   '("/" . meow-keypad-describe-key)
   '("?" . meow-cheatsheet))
  (meow-normal-define-key
   '("0" . meow-expand-0)
   '("9" . meow-expand-9)
   '("8" . meow-expand-8)
   '("7" . meow-expand-7)
   '("6" . meow-expand-6)
   '("5" . meow-expand-5)
   '("4" . meow-expand-4)
   '("3" . meow-expand-3)
   '("2" . meow-expand-2)
   '("1" . meow-expand-1)
   '("-" . negative-argument)
   '(";" . meow-reverse)
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("[" . meow-beginning-of-thing)
   '("]" . meow-end-of-thing)
   '("a" . meow-append)
   '("A" . meow-open-below)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("x" . meow-delete) ; d
   '("X" . meow-backward-delete) ; D
   '("e" . meow-next-word)
   '("E" . meow-next-symbol)
   '("f" . meow-find)
   '("g" . meow-cancel-selection)
   '("G" . meow-grab)
   '("h" . meow-left)
   '("H" . meow-left-expand)
   '("i" . meow-insert)
   '("I" . meow-open-above)
   '("j" . meow-next)
   '("J" . meow-next-expand)
   '("k" . meow-prev)
   '("K" . meow-prev-expand)
   '("l" . meow-right)
   '("L" . meow-right-expand)
   '("m" . meow-join)
   '("n" . meow-search)
   '("o" . meow-block)
   '("O" . meow-to-block)
   '("p" . meow-yank)
   '("q" . meow-quit)
   '("S" . meow-goto-line) ; Q/X
   '("P" . meow-replace) ; r
   '("r" . meow-swap-grab) ; R
   '("d" . meow-kill) ; s
   '("t" . meow-till)
   '("u" . meow-undo)
   '("Z" . meow-undo-in-selection) ; U
   '("v" . meow-visit)
   '("w" . meow-mark-word)
   '("W" . meow-mark-symbol)
   '("s" . meow-line) ; x
   '("y" . meow-save)
   '("R" . meow-sync-grab) ; Y
   '("z" . meow-pop-selection)
   '("'" . repeat)
   '("<escape>" . ignore)
   ;;; new keys
   '("/" . my/meow-comment)
   '("X" . my/meow-delete-region)
   '("\\s" . avy-goto-line)
   '("\\f" . avy-goto-char)
   '("\\w" . ace-window)
   '("U" . my/meow-redo)
   ;; page movement
   '("<next>" . meow-page-down)
   '("<prior>" . meow-page-up)
   ;; indent by char
   '("<" . my/meow-indent-left)
   '(">" . my/meow-indent-right)
   ;; indent by tab 
   '("TAB" . meow-indent)
   '("<backtab>" . my/meow-indent-left)
   ;; easier macros
   '("(" . meow-start-kmacro)
   '(")" . meow-end-kmacro)
   '("+" . meow-start-kmacro-or-insert-counter)
   '("=" . meow-end-or-call-kmacro)))

(defun my/meow-comment ()
  "Indent region to the right, or current line if no region is active."
  (interactive)
  (when (meow--allow-modify-p)
    (if (use-region-p)
        (comment-or-uncomment-region (region-beginning) (region-end))
      (comment-or-uncomment-region (line-beginning-position) (line-end-position))))
  (setq deactivate-mark nil))

(defun my/meow-indent-right ()
  "Indent region to the right, or current line if no region is active."
  (interactive)
  (when (meow--allow-modify-p)
    (if (use-region-p)
        (indent-rigidly-right (region-beginning) (region-end))
      (indent-rigidly-right (line-beginning-position) (line-end-position))))
  (setq deactivate-mark nil))

(defun my/meow-indent-left ()
  "Indent region to the left, or current line if no region is active."
  (interactive)
  (when (meow--allow-modify-p)
    (if (use-region-p)
        (indent-rigidly-left (region-beginning) (region-end))
      (indent-rigidly-left (line-beginning-position) (line-end-position))))
  (setq deactivate-mark nil))

(defun my/meow-delete-region ()
  "Deletes active region."
  (interactive)
  (when (meow--allow-modify-p)
    (if (use-region-p)
        (call-interactively #'delete-active-region)
      (call-interactively #'delete-backward-char))))

(defun my/meow-redo ()
  "Cancel current selection then redo."
  (interactive)
  (when (region-active-p)
    (meow--cancel-selection))
  (meow--execute-kbd-macro "M-_"))
