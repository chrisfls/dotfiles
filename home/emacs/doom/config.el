;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "John Doe"
      user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-gruvbox)
(setq doom-gruvbox-dark-variant "hard")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; treemacs
(use-package! treemacs
  :when (modulep! :ui treemacs)
  :init
  (setq treemacs-git-mode 'deferred)
  (setq treemacs-position 'right)
  :config
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  ;; fix sidebar +- signs
  (with-eval-after-load 'doom-themes
    (unless (display-graphic-p)
      (setq doom-themes-treemacs-theme "Default"))))

;; meow
(use-package! meow
  :when t
  :demand t
  :init
  (meow-global-mode +1)
  ;; make cursor change work on wt
  (defun meow--set-cursor-type (type) (setq cursor-type type))
  :config
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
  (setq meow-cheatsheet-physical-layout meow-cheatsheet-physical-layout-iso)
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
   '("d" . meow-delete)
   '("D" . meow-backward-delete)
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
   '("Q" . meow-goto-line)
   '("r" . meow-replace)
   '("R" . meow-swap-grab)
   '("s" . meow-kill)
   '("t" . meow-till)
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   '("v" . meow-visit)
   '("w" . meow-mark-word)
   '("W" . meow-mark-symbol)
   '("x" . meow-line)
   '("X" . meow-goto-line)
   '("y" . meow-save)
   '("Y" . meow-sync-grab)
   '("z" . meow-pop-selection)
   '("'" . repeat)
   '("<escape>" . ignore))
  ;; custom settings
  (setq my-meow-exit-with-cg 'insert) ; 'insert 'insert-and-beacon
  (setq my-meow-bindings t)
  (setq my-meow-remap-help t)
  (setq my-meow-use-doom-leader nil)
  ;; keypad indicator for terminal
  (unless (display-graphic-p)
    (setq meow-cursor-type-keypad 'hbar))
  ;; remap help
  (map! :map meow-keymap [remap describe-key] ; C-h k
    :when my-meow-remap-help
    #'helpful-key)
  ;; exit from insert mode with C-g
  (map! :map meow-insert-state-keymap
    :when (memq my-meow-exit-with-cg '(insert insert-and-beacon))
    "C-g" #'meow-insert-exit)
  ;; exit from beacon mode with C-g
  (map! :map meow-beacon-state-keymap
    :when (eq my-meow-exit-with-cg 'insert-and-beacon)
    "C-g" #'meow-grab)
  ;; integrate with doom leader key
  (map! :map meow-normal-state-keymap
    :when my-meow-use-doom-leader
    doom-leader-key doom-leader-map)
  (map! :map meow-motion-state-keymap
    :when my-meow-use-doom-leader
    doom-leader-key doom-leader-map)
  (map! :map meow-beacon-state-keymap
    :when my-meow-use-doom-leader
    doom-leader-key nil)
  (map! :leader
    :when my-meow-use-doom-leader
    ;; keypad mode access with +leader
    "l" #'meow-keypad
    ;; SPC j/k will run the original command in MOTION state.
    "j" "H-j"
    "k" "H-k"
    ;; Use SPC (0-9) for digit arguments.
    "1" #'meow-digit-argument
    "2" #'meow-digit-argument
    "3" #'meow-digit-argument
    "4" #'meow-digit-argument
    "5" #'meow-digit-argument
    "6" #'meow-digit-argument
    "7" #'meow-digit-argument
    "8" #'meow-digit-argument
    "9" #'meow-digit-argument
    "0" #'meow-digit-argument
    "/" #'meow-keypad-describe-key
    "?" #'meow-cheatsheet)
  ;; custom keybindings
  (map! :map meow-normal-state-keymap
    :when my-meow-bindings
    "n" #'avy-goto-char))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; fix mouse scroll wheel
(map! :unless (display-graphic-p)
  "<mouse-4>" (cmd! (scroll-down 2))
  "<mouse-5>" (cmd! (scroll-up 2)))

(setq completion-styles '(initials orderless basic))
(setq read-file-name-completion-ignore-case t)

;; adaptive-wrap
(+global-word-wrap-mode +1)

;; visual-fill-column
(setq-default display-fill-column-indicator-column 79)

(defun my/fill-column-indicator (col)
  (setq-local display-fill-column-indicator-column col)
  (display-fill-column-indicator-mode t))

(add-hook! (text-mode prog-mode)
  (my/fill-column-indicator 79))

(add-hook! elm-mode
  (my/fill-column-indicator 119))

(add-hook! fsharp-mode
  (my/fill-column-indicator 99))
