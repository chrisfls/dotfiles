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

;; There are two ways to load a theme. Both assume thOIe theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-gruvbox
      doom-gruvbox-dark-variant "hard")

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

;;; doom-dashboard

(defun my/dashboard-draw-ascii-banner-fn ()
  (let* ((banner
          '("                                           "
            "███████╗███╗   ███╗ █████╗  ██████╗███████╗"
            "██╔════╝████╗ ████║██╔══██╗██╔════╝██╔════╝"
            "█████╗  ██╔████╔██║███████║██║     ███████╗"
            "██╔══╝  ██║╚██╔╝██║██╔══██║██║     ╚════██║"
            "███████╗██║ ╚═╝ ██║██║  ██║╚██████╗███████║"
            "╚══════╝╚═╝     ╚═╝╚═╝  ╚═╝ ╚═════╝╚══════╝"
            "                                           "))
         (longest-line (apply #'max (mapcar #'length banner))))
    (put-text-property
     (point)
     (dolist (line banner (point))
       (insert (+doom-dashboard--center
                +doom-dashboard--width
                (concat
                 line (make-string (max 0 (- longest-line (length line)))
                                   32)))
               "\n"))
     'face 'doom-dashboard-banner)))

(setq +doom-dashboard-ascii-banner-fn #'my/dashboard-draw-ascii-banner-fn)

;;; fix scroll wheel

(map! :unless (display-graphic-p)
  "<mouse-4>" (cmd! (scroll-down 2))
  "<mouse-5>" (cmd! (scroll-up 2)))

;;; doom-modeline

(use-package! doom-modeline
  :config
  (defun doom-modeline--evil-ryo ()
    "The current ryo-modal state which is enabled by the command `ryo-modal-mode'."
    (cond 
      ;; visual
      ((and ryo-modal-mode mark-active)
        (doom-modeline--modal-icon "VISUAL" 'doom-modeline-evil-visual-state "Ryo modal" "add_circle" "✪"))
      ;; normal
      (ryo-modal-mode
        (doom-modeline--modal-icon "NORMAL" 'doom-modeline-evil-normal-state "Ryo modal" "add_circle" "✪"))
      ;; insert
      (t
        (doom-modeline--modal-icon "INSERT" 'doom-modeline-evil-insert-state "Holy mode"))))
  (doom-modeline-def-segment modals
    (when doom-modeline-modal
      (concat
        (doom-modeline-spc)
        (doom-modeline--evil-ryo)
        (doom-modeline-spc)))))

;;; treemacs

(setq treemacs-git-mode 'deferred
      treemacs-width 30
      treemacs-recenter-after-file-follow t
      treemacs-recenter-after-tag-follow t)

(after! treemacs
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-git-commit-diff-mode t)
  ;; fix sidebar +- signs
  (with-eval-after-load 'doom-themes
    (unless (display-graphic-p)
      (setq doom-themes-treemacs-theme "Default"))))

;;; which-key

(use-package! which-key
  :config
  (setq which-key-show-early-on-C-h t
        which-key-idle-delay 0.05
        which-key-idle-secondary-delay 0.05
        which-key-popup-type 'side-window
        which-key-side-window-location 'right)
  (push '((nil . "ryo:.*:") . (nil . "")) which-key-replacement-alist))

;;; all-the-icons-nerd-fonts

;; TODO: add style icon to nerd-mdi
;; (use-package! all-the-icons-nerd-fonts
;;   :after all-the-icons
;;   :demand t
;;   :config
;;   (all-the-icons-nerd-fonts-prefer))

;;; undo-tree

(use-package! undo-tree
  :demand t
  :init)

;;; point-undo

(use-package! point-undo
  :demand t
  :init)

;;; smartparens
(use-package! smartparens)

;;; my commands
(use-package! char-occurrence)
(use-package! open-line)
(use-package! region-boundaries)
(use-package! repeat-nth :demand t)
(use-package! save-kmacro)
(use-package! secondary-selection)
(use-package! smarter-commands)
(use-package! text-object)

;;; ryo-modal
(use-package! ryo-modal
  :demand t
  :bind
  ("<escape>" . ryo-modal-mode)
  :config
  (require 'evil-core)
  (evil-esc-mode 1)
  (setq-default cursor-type 'bar)
  (setq-default blink-cursor-blinks 0)
  (setq ryo-modal-cursor-type 'block)
  (ryo-modal-keys
    ("<escape>" keyboard-escape-quit)
    ("0" repeat-ten :norepeat t)
    ("1" ryo-modal-repeat :norepeat t)
    ("2" repeat-two :norepeat t)
    ("3" repeat-three :norepeat t)
    ("4" repeat-four :norepeat t)
    ("5" repeat-five :norepeat t)
    ("6" repeat-six :norepeat t)
    ("7" repeat-seven :norepeat t)
    ("8" repeat-eight :norepeat t)
    ("9" repeat-nine :norepeat t)
    ;;; ergonomics
    ("C-@" "M-x" :norepeat t) ; [CTRL]
    ("C-SPC" "M-x" :norepeat t) ; [CTRL]
    ;;; movement
    ("l" forward-char)
    ("L" end-of-line) ; [SHIFT]
    ("h" backward-char)
    ("H" back-to-indentation-or-beginning-of-line) ; [SHIFT]
    ("j" next-line)
    ("J" scroll-down-command) ; [SHIFT]
    ("k" previous-line)
    ("K" scroll-up-command) ; [SHIFT]
    ("w" forward-word)
    ("W" sp-forward-symbol) ; [SHIFT]
    ("b" backward-word)
    ("B" sp-backward-symbol) ; [SHIFT]
    ("o" match-sexp) ; TODO: "m"
    ("O" sp-beginning-of-next-sexp) ; TODO: REMOVE?
    ("]" (("p" forward-paragraph)
          ("d" end-of-defun)
          ("b" end-of-buffer)
          ("w" backward-word)
          ("l" sp-backward-symbol)
          ;; ("r" backward-round)
          ;; ("c" backward-curly)
          ;; ("s" backward-square)
          ))
    ("[" (("p" backward-paragraph)
          ("d" beginning-of-defun)
          ("b" beginning-of-buffer)
          ("w" forward-word)
          ("l" sp-forward-symbol)
          ;; ("r" forward-round)
          ;; ("c" forward-curly)
          ;; ("s" forward-square)
          ))
    ;; ("," (("p" select-inner-paragraph)
    ;;       ("d" select-inner-defun)
    ;;       ("b" select-whole-buffer)
    ;;       ("w" select-word)
    ;;       ("l" symbol)
    ;;       ("r" select-inner-round)
    ;;       ("c" select-inner-curly)
    ;;       ("s" select-inner-square)))
    ;; ("." (("p" select-outer-paragraph)
    ;;       ("d" select-outer-defun)
    ;;       ("b" select-whole-buffer)
    ;;       ("w" select-word)
    ;;       ("l" symbol)
    ;;       ("r" select-outer-round)
    ;;       ("c" select-outer-curly)
    ;;       ("s" select-outer-square)))
    ("f" isearch-repeat-forward-region :norepeat t) ; [REPEATS] TODO: mix with isearch-forward-region
    ("F" isearch-exit :norepeat t) ; [SHIFT]
    ("n" go-to-char-forward :norepeat t) ; [REPEATS]
    ("N" avy-goto-char :norepeat t) ; [SHIFT]
    ("z" point-undo)
    ;;; mark
    ("SPC" toggle-mark)
    ("g" "C-g")
    ("G" grab-region)
    (";" exchange-point-and-mark)
    ("s" mark-whole-line-to-next)
    ;;; manipulation
    ("a" go-to-end-of-region :exit t :norepeat t)
    ("A" open-line-down :exit t :norepeat t) ; [SHIFT]
    ("i" go-to-beginning-of-region :exit t :norepeat t)
    ("I" open-line-up :exit t :norepeat t) ; [SHIFT]
    ("d" delete-forward-char-or-region) 
    ("c" delete-char :exit t :norepeat t) ; TODO: REMOVE?
    ("C" recenter :norepeat t) ; [SHIFT]
    ("x" delete-backward-char-or-kill-region)
    ("y" kill-ring-save)
    ("p" yank)
    ("," join-line) ; TOOD: "m" (under j which does the same thing in vim)
    ("u" undo-tree-undo)
    ("r" swap-grab) ; TODO: 
    ("R" sync-grab-content :norepeat t) ; [SHIFT]
    ;;; macros
    ("m" start-or-cancel-macro :norepeat t) ; TODO: "r" (record)
    ("M" start-or-save-macro :norepeat t) ; [SHIFT] TODO: "R"
    ("e" execute-macro :norepeat t) ; [REPEATS]
    ;;; reverse
    ("-" (("O" sp-beginning-of-previous-sexp)
          ("f" isearch-repeat-backward-region :norepeat t) ; [REPEATS]
          ("n" go-to-char-backward :norepeat t) ; [REPEATS]
          ("z" point-redo)
          ("s" mark-whole-line-to-previous)
          ("u" undo-tree-redo)
          ("r" swap-grab-content)
          ("R" sync-region-content :norepeat t)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq completion-styles '(orderless)
      orderless-matching-styles '(orderless-flex orderless-literal orderless-regexp)
      read-file-name-completion-ignore-case t
      avy-style 'de-bruijn)

;; adaptive-wrap
(+global-word-wrap-mode +1)

;; visual-fill-column
(setq-default display-fill-column-indicator-column 79)

(defun my/fill-column-indicator (col)
  (setq-local display-fill-column-indicator-column col)
  (display-fill-column-indicator-mode t))

(add-hook! (text-mode prog-mode)
  (my/fill-column-indicator 79)
  (ryo-modal-mode))

(add-hook! elm-mode
  (my/fill-column-indicator 119))

(add-hook! fsharp-mode
  (my/fill-column-indicator 99))
