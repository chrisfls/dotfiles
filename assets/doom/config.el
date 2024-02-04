;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Christian Ferraz"
      user-mail-address ""
      org-directory "~/Desktop/org/")

;;;;;;;;;;;;;;;;;;
;;; APPEARANCE ;;;
;;;;;;;;;;;;;;;;;;

;;;
;;; immediate
;;;

;; setup color theme
(setq doom-theme 'doom-ir-black) ; doom-outrun-electric

;; setup fonts (TODO: doom-big-font doom-symbol-font doom-serif-font)
(setq doom-font (font-spec :family "Jetbrains Mono NFM" :size (* 12.0 1.5))
      doom-variable-pitch-font (font-spec :family "Noto Sans" :size (* 13.0 1.5)))

;; disable dashboard ascii art
(setq +doom-dashboard-ascii-banner-fn nil)

;; setup frame opacity
(set-frame-parameter nil 'alpha-background 90)
(add-to-list 'default-frame-alist '(alpha-background . 90))

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

(add-hook! doom-after-modules-config

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

(after! highlight-indent-guides
  (setq highlight-indent-guides-method 'column
        highlight-indent-guides-responsive 'top
        highlight-indent-guides-auto-odd-face-perc 25
        highlight-indent-guides-auto-even-face-perc 25
        highlight-indent-guides-auto-character-face-perc 50)
  (highlight-indent-guides-auto-set-faces))

(after! recentf
 (add-to-list 'recentf-exclude "~/.config/emacs/.local"))

(after! vertico
  (setq vertico-count 10
        vertico-scroll-margin 0))

;;;;;;;;;;;;;;;;;;;;;;
;;; EXTRA PACKAGES ;;;
;;;;;;;;;;;;;;;;;;;;;;

(use-package! macrursors
  ;; pending: macrursors-mark-next-line macrursors-mark-previous-line
  :hook (macrursors-pre-finish-hook . company-mode)
        (macrursors-post-finish-hook . company-mode)
  :config
  (define-prefix-command 'macrursors-mark-map)
  (setq macrursors-match-cursor-style nil))

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

(defun my/macrursors-select ()
  (interactive)
  (if (use-region-p)
      (macrursors-select)
    (macrursors-select-clear)))

;;;;;;;;;;;;;;;;
;;; BINDINGS ;;;
;;;;;;;;;;;;;;;;

(map!
  "C-("   #'macrursors-start
  "M-n"   #'macrursors-mark-next-instance-of
  "M-p"   #'macrursors-mark-previous-instance-of
  "M-s n" #'macrursors-mark-next-from-isearch
  "M-s p" #'macrursors-mark-previous-from-isearch
  "M-s m" #'macrursors-mark-from-isearch)

(customize-set-variable 'macrursors-apply-keys "C-)")

(map! :leader
      (:prefix-map ("m" . "multiple-cursors")
       :desc "Select"                      "m"     #'my/macrursors-select
       :desc "Clear selection"             "c"     #'macrursors-select-clear
       ;;; Mark-all
       :desc "Mark all sentences"          "."     #'macrursors-mark-all-sentences
       :desc "Mark all sexps"              "e"     #'macrursors-mark-all-sexps
       :desc "Mark all defuns"             "f"     #'macrursors-mark-all-defuns
       :desc "Mark all lines or instances" "l"     #'macrursors-mark-all-lines-or-instances
       :desc "Mark all numbers"            "n"     #'macrursors-mark-all-numbers
       :desc "Mark all symbols"            "s"     #'macrursors-mark-all-symbols
       :desc "Mark all urls"               "u"     #'macrursors-mark-all-urls
       :desc "Mark all words"              "w"     #'macrursors-mark-all-words))
