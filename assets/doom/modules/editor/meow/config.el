;;; editor/meow/config.el -*- lexical-binding: t; -*-

(setq doom-leader-key "\\"
      doom-leader-alt-key "C-\\"
      doom-localleader-key "\\ l"
      doom-localleader-alt-key "C-\\ l")

(map! 
  "M-;" 'comment-or-uncomment-region
  "C-M-\\" 'indent-for-tab-command
   "M-<up>" 'scroll-down-line
   "M-<down>" 'scroll-up-line)

(use-package! meow
  :hook (doom-after-modules-config . meow-global-mode)
  :demand t
  :bind (:map meow-normal-state-keymap
         ("\\" . 'doom/leader)
         :map meow-motion-state-keymap
         ("\\" . 'doom/leader))
  :config
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty
        meow-cursor-type-insert '(bar . 4)
        meow-cursor-type-region-cursor '(bar . 4)
        meow-cursor-type-keypad 'hollow
        meow-use-clipboard t
        meow-keypad-leader-dispatch "C-x"
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
   '("x" . meow-delete) ; d
   '("X" . meow-backward-delete) ; D
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
   '("v" . meow-visit) ; v
   '("m" . meow-mark-word) ; w
   '("M" . meow-mark-symbol) ; W
   '("s" . meow-line) ; x
   '(":" . meow-goto-line) ; X
   '("y" . meow-save) ; y
   '("Y" . meow-sync-grab) ; Y
   '("z" . meow-pop-selection) ; z
   '("." . repeat) ; '
   '("<escape>" . ignore) ; <escape>
   ;; new keys
   '("" . meow-indent)
   '("<prior>" . meow-page-up)
   '("<next>" . meow-page-down)
   '(";" . meow-comment)))
