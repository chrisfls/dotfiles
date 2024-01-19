;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Christian Ferraz"
      user-mail-address "")

(setq org-directory "~/Desktop/org/")

(setq doom-font (font-spec :family "Jetbrains Mono NFM" :size (* 12.0 1.5)) ; :weight 'semi-light
      doom-variable-pitch-font (font-spec :family "Noto Sans" :size (* 13.0 1.5))
      ;;doom-big-font
      ;;doom-symbol-font
      ;;doom-serif-font
      )

(setq doom-theme 'doom-gruvbox ; 'doom-gruvbox 'doom-dracula 'doom-monokai-classic 'doom-tokyo-night
      doom-gruvbox-dark-variant "hard"
      +doom-dashboard-ascii-banner-fn nil)

(setq doom-leader-alt-key "C-l"
      doom-localleader-alt-key "C-S-l")

(setq display-line-numbers-type 'relative
      scroll-margin 3
      scroll-step 3)

(setq-default cursor-type '(bar . 4)
              display-fill-column-indicator-column 79
              display-fill-column-indicator-character ?╎)

(global-display-fill-column-indicator-mode 1)
;;(+global-word-wrap-mode 1) ; TODO: set word wrap to 120

;;(use-package! key-chord
;;  :init
;;  (key-chord-mode 1)
;;  :config
;;  (setq key-chord-two-keys-delay 0.05)
;;  (key-chord-define-global "fd" 'ignore))

(use-package! meow
  :init
  (meow-global-mode 1)
  :config
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty
        meow-cursor-type-default '(hbar . 4)
        meow-cursor-type-normal '(hbar . 4)
        meow-cursor-type-motion '(hbar . 4)
        meow-cursor-type-beacon '(hbar . 4)
        meow-cursor-type-insert '(bar . 4)
        meow-cursor-type-keypad 'hollow
        meow-use-clipboard t)
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
   '("/" . meow-cheatsheet) ; ?
   ;; new keys
   '("l" . meow-keypad-start) ; l
   '("L" . meow-keypad-start)) ; L
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
   '("<backtab>" . meow-indent)
   '("<prior>" . meow-page-up)
   '("<next>" . meow-page-down)
   '("<" . indent-rigidly-left-to-tab-stop)
   '(">" . indent-rigidly-right-to-tab-stop)
   '(";" . meow-comment)))

(after! which-key
  (setq which-key-popup-type 'side-window
        which-key-side-window-location 'top))
