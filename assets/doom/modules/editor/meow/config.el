;;; editor/meow/config.el -*- lexical-binding: t; -*-

(setq doom-leader-key "C-c"
      doom-leader-alt-key "C-c"
      doom-localleader-key "C-c l"
      doom-localleader-alt-key "C-c l")

(map!
  "M-;" 'custom/comment-or-uncomment-region
  "C-M-\\" 'custom/indent-rigidly-right-to-tab-stop)

;; TODO: bind 
;;  avy-goto-line
;;  avy-goto-char

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
   '("j" . "H-j") ; j
   '("k" . "H-k") ; k
   ;; Use SPC (0-9) for digit arguments.
   '("1" . meow-digit-argument) ; 1
   '("2" . meow-digit-argument) ; 2
   '("3" . meow-digit-argument) ; 3
   '("4" . meow-digit-argument) ; 4
   '("5" . meow-digit-argument) ; 5
   '("6" . meow-digit-argument) ; 6
   '("7" . meow-digit-argument) ; 7
   '("8" . meow-digit-argument) ; 8
   '("9" . meow-digit-argument) ; 9
   '("0" . meow-digit-argument) ; 0
   '("?" . meow-keypad-describe-key) ; /
   '("/" . meow-cheatsheet)) ; ?
  (meow-normal-define-key
   '("0" . meow-expand-0) ; 0
   '("9" . meow-expand-9) ; 9
   '("8" . meow-expand-8) ; 8
   '("7" . meow-expand-7) ; 7
   '("6" . meow-expand-6) ; 6
   '("5" . meow-expand-5) ; 5
   '("4" . meow-expand-4) ; 4
   '("3" . meow-expand-3) ; 3
   '("2" . meow-expand-2) ; 2
   '("1" . meow-expand-1) ; 1
   '("-" . negative-argument) ; -
   '("," . meow-reverse) ; ;
   '("{" . meow-inner-of-thing) ; ,
   '("}" . meow-bounds-of-thing) ; .
   '("[" . meow-beginning-of-thing) ; [
   '("]" . meow-end-of-thing) ; ]
   '("a" . meow-append) ; a
   '("o" . meow-open-below) ; A
   '("w" . meow-back-word) ; b
   '("W" . meow-back-symbol) ; B
   '("c" . meow-change) ; c
   '("DEL" . custom/meow-delete) ; d
   '("<backspace>" . custom/meow-backward-delete) ; D
   '("e" . meow-next-word) ; e
   '("E" . meow-next-symbol) ; E
   '("f" . meow-find) ; f
   '("g" . meow-cancel-selection) ; g
   '("G" . meow-grab) ; G
   '("h" . meow-left) ; h
   '("H" . meow-left-expand) ; H
   '("i" . meow-insert) ; i
   '("O" . meow-open-above) ; I
   '("j" . meow-next) ; j
   '("J" . meow-next-expand) ; J
   '("k" . meow-prev) ; k
   '("K" . meow-prev-expand) ; K
   '("l" . meow-right) ; l
   '("L" . meow-right-expand) ; L
   '("S" . meow-join) ; m
   '("n" . meow-search) ; n
   '("b" . meow-block) ; o
   '("B" . meow-to-block) ; O
   '("p" . meow-yank) ; p
   '("q" . meow-quit) ; q
   '("P" . meow-replace) ; r
   '("r" . meow-swap-grab) ; R
   '("d" . meow-kill) ; s
   '("t" . meow-till) ; t
   '("u" . meow-undo) ; u
   '("U" . meow-undo-in-selection) ; U
   '("s" . meow-visit) ; v
   '("v" . meow-mark-word) ; v
   '("V" . meow-mark-symbol) ; V
   '("x" . meow-line) ; x
   '(":" . meow-goto-line) ; X
   '("y" . meow-save) ; y
   '("Y" . meow-sync-grab) ; Y
   '("z" . meow-pop-selection) ; z
   '("." . repeat) ; '
   '("<escape>" . ignore) ; <escape>
   ;;; new keys
   '(";" . meow-comment)
   '("S-<delete>" . custom/meow-backward-delete)
   ;; page movement
   '("<next>" . meow-page-down)
   '("<prior>" . meow-page-up)
   ;; indent by char
   '("<" . custom/indent-rigidly-left)
   '(">" . custom/indent-rigidly-right)
   ;; indent by tab 
   '("TAB" . meow-indent)
   '("<backtab>" . custom/indent-rigidly-left-to-tab-stop)
   ;; easier macros
   '("(" . meow-start-kmacro)
   '(")" . meow-end-kmacro)
   '("ms" . meow-start-kmacro-or-insert-counter)
   '("me" . meow-end-or-call-kmacro)))

(defun custom/comment-or-uncomment-region ()
  "Indent region to the right, or current line if no region is active."
  (interactive)
  (if (use-region-p)
      (comment-or-uncomment-region (region-beginning) (region-end))
    (comment-or-uncomment-region (line-beginning-position) (line-end-position))))

(defun custom/indent-rigidly-right-to-tab-stop ()
  "Indent region to the right, or current line if no region is active."
  (interactive)
  (if (use-region-p)
      (indent-rigidly-right-to-tab-stop (region-beginning) (region-end))
    (indent-rigidly-right-to-tab-stop (line-beginning-position) (line-end-position))))

(defun custom/indent-rigidly-left-to-tab-stop ()
  "Indent region to the left, or current line if no region is active."
  (interactive)
  (if (use-region-p)
      (indent-rigidly-left-to-tab-stop (region-beginning) (region-end))
    (indent-rigidly-left-to-tab-stop (line-beginning-position) (line-end-position))))

(defun custom/indent-rigidly-right ()
  "Indent region to the right, or current line if no region is active."
  (interactive)
  (if (use-region-p)
      (indent-rigidly-right (region-beginning) (region-end))
    (indent-rigidly-right (line-beginning-position) (line-end-position))))

(defun custom/indent-rigidly-left ()
  "Indent region to the left, or current line if no region is active."
  (interactive)
  (if (use-region-p)
      (indent-rigidly-right (region-beginning) (region-end))
    (indent-rigidly-left (line-beginning-position) (line-end-position))))

(defun custom/meow-delete ()
  "Backward delete one char."
  (interactive)
  (when (meow--allow-modify-p)
    (if (use-region-p)
        (call-interactively #'delete-active-region)
      (call-interactively #'delete-backward-char))))

(defun custom/meow-backward-delete ()
  "Backward delete one char."
  (interactive)
  (when (meow--allow-modify-p)
    (if (use-region-p)
        (call-interactively #'delete-active-region)
      (meow--execute-kbd-macro meow--kbd-delete-char))))
