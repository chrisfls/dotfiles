;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Christian Ferraz"
      user-mail-address "")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
(setq doom-font (font-spec :family "Jetbrains Mono NFM" :size (* 12.0 1.5) :weight 'semi-light)
      doom-variable-pitch-font (font-spec :family "Noto Sans" :size (* 13.0 1.5)))

;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'sanityinc-tomorrow-bright)  ; doom-outrun-electric doom-ir-black

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Desktop/org/")


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

;;;;;;;;;;;;;;;;;;
;;; APPEARANCE ;;;
;;;;;;;;;;;;;;;;;;

;;;
;;; immediate
;;;

;; disable dashboard ascii art
(setq +doom-dashboard-ascii-banner-fn nil)

;; setup frame opacity (use 90 if theme is not black)
(set-frame-parameter nil 'alpha-background 80)
(add-to-list 'default-frame-alist '(alpha-background . 80))

;; setup cursor
(setq blink-cursor-blinks 0)
(setq-default cursor-type '(bar . 4))
(blink-cursor-mode 1)

;; setup indent guides
(after! highlight-indent-guides
  (setq highlight-indent-guides-method 'column
        highlight-indent-guides-responsive 'top
        highlight-indent-guides-auto-odd-face-perc 25
        highlight-indent-guides-auto-even-face-perc 25
        highlight-indent-guides-auto-character-face-perc 50)
  (highlight-indent-guides-auto-set-faces))

;; setup rulers
(setq-default display-fill-column-indicator-column 79
              display-fill-column-indicator-character ?\s
              fill-column 80)

;;;
;;; deferred
;;;

;; indent guides and rulers
(after! highlight-indent-guides
  ;; setup indent guides colors
  (setq highlight-indent-guides-method 'column
        highlight-indent-guides-responsive 'top
        highlight-indent-guides-auto-odd-face-perc 25
        highlight-indent-guides-auto-even-face-perc 25
        highlight-indent-guides-auto-character-face-perc 50)
  (highlight-indent-guides-auto-set-faces)
  ;; setup ruler colors
  (set-face-attribute 'fill-column-indicator nil
    :background (face-attribute 'highlight-indent-guides-even-face
                                :background nil
                                'default)
    :foreground nil))

;;;;;;;;;;;;;;;;
;;; BEHAVIOR ;;;
;;;;;;;;;;;;;;;;

;;;
;;; immediate
;;;

;; setup leader key 
(setq doom-leader-alt-key "C-c"
      doom-leader-key "C-c"
      doom-localleader-alt-key "C-c SPC"
      doom-localleader-key "C-c SPC")

;; setup editor scroll
(setq scroll-conservatively 10000
      scroll-margin 3
      scroll-preserve-screen-position 1
      scroll-step 3)

;; setup mouse scrolling
(setq mouse-wheel-follow-mouse 't
      mouse-wheel-progressive-speed nil
      mouse-wheel-scroll-amount '(3 ((shift) . 3)))

;; disable ring bell sound
(set ring-bell-function 'ignore)

;; setup word wrap
(setq +word-wrap-extra-indent 'single)
(+global-word-wrap-mode 1)
(global-display-fill-column-indicator-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;
;;; PACKAGE SETTINGS ;;;
;;;;;;;;;;;;;;;;;;;;;;;;

(after! recentf
 (add-to-list 'recentf-exclude "~/.config/emacs/.local"))

(after! vertico
  (setq vertico-count 10
        vertico-scroll-margin 0))

;;;;;;;;;;;;;;;;;;;;;;
;;; EXTRA PACKAGES ;;;
;;;;;;;;;;;;;;;;;;;;;;

;; (use-package! vertico-buffer :demand t
;;   :init
;;   (vertico-buffer-mode 1)
;;   :config
;;   (setq vertico-buffer-display-action
;;       '(display-buffer-in-side-window
;;         (window-height . 12)
;;         (side . top))) )

;;;;;;;;;;;;;;;;;;;;;;;;;
;;; LANGUAGE SETTINGS ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook! typescript-mode
  (deno-fmt-mode 1))

(add-hook! js2-mode
  (deno-fmt-mode 1))

;;;;;;;;;;;;;;;;
;;; COMMANDS ;;;
;;;;;;;;;;;;;;;;

;; (defun my/comment ()
;;   (interactive)
;;   (if (use-region-p)
;;       (comment-or-uncomment-region (region-beginning) (region-end))
;;     (comment-or-uncomment-region (line-beginning-position) (line-end-position)))
;;   (setq deactivate-mark nil))
