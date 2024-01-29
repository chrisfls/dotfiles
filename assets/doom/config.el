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
(setq doom-theme 'doom-outrun-electric)

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

(define-prefix-command 'macrursors-mark-map)
(use-package! macrursors
  ;; cmd  - macrursors-apply-command
  ;; cmd  - macrursors-early-quit
  ;; cmd  - macrursors-end
  ;; cmd  - macrursors-mark-all-defuns
  ;; cmd  - macrursors-mark-all-instances-of
  ;; cmd  - macrursors-mark-all-lines
  ;; cmd  - macrursors-mark-all-lines-or-instances
  ;; cmd  - macrursors-mark-all-lists
  ;; cmd  - macrursors-mark-all-numbers
  ;; cmd  - macrursors-mark-all-sentences
  ;; cmd  - macrursors-mark-all-sexp
  ;; cmd  - macrursors-mark-all-sexps
  ;; cmd  - macrursors-mark-all-symbols
  ;; cmd  - macrursors-mark-all-urls
  ;; cmd  - macrursors-mark-all-words
  ;; cmd  - macrursors-mark-from-isearch
  ;; cmd  - macrursors-mark-next-from-isearch
  ;; cmd  - macrursors-mark-next-instance-of
  ;; cmd  - macrursors-mark-next-line
  ;; cmd  - macrursors-mark-previous-from-isearch
  ;; cmd  - macrursors-mark-previous-instance-of
  ;; cmd  - macrursors-mark-previous-line
  ;; cmd  - macrursors-start
  ;; face - macrursors-cursor-bar-face
  ;; face - macrursors-cursor-face
  ;; face - macrursors-region-face
  ;; hook - macrursors-post-finish-hook
  ;; hook - macrursors-pre-finish-hook
  ;; mode - macrursors-mode
  ;; var  - macrursors-apply-keys
  ;; var  - macrursors-match-cursor-style
  ;; var  - macrursors-mode-line
  :hook (macrursors-pre-finish-hook . company-mode)
        (macrursors-post-finish-hook . company-mode))

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

;;;;;;;;;;;;;;;;
;;; BINDINGS ;;;
;;;;;;;;;;;;;;;;

(map!
  "M-n" #'macrursors-mark-next-instance-of
  "M-p" #'macrursors-mark-previous-instance-of
  ;"C-;" 'macrursors-mark-map
  )

(map! :map macrursors-mark-map
  ;; apply/quit
  "RET" #'macrursors-mark-all-lines-or-instances
  "."   #'macrursors-mark-all-sentences
  "f"   #'macrursors-mark-all-defuns
  "h"   #'macrursors-mark-all-lines
  "l"   #'macrursors-mark-all-lists
  "n"   #'macrursors-mark-all-numbers
  "w"   #'macrursors-mark-all-words
  "s"   #'macrursors-mark-all-symbols
  "e"   #'macrursors-mark-all-sexps
  "a"   #'macrursors-early-quit)

;; (map! :leader
;;       (:prefix-map ("m" . "multiple-cursors")
;;        ;;:desc "Add cursor w/mouse" "<mouse-1>" #'mc/add-cursor-on-click
;;        :desc "Select"                 "SPC"       #'macrursors-select
;;        ;;; mark all
;;        :desc "Mark all lines"         "h"         #'macrursors-mark-all-lines
;;        :desc "Mark all"               "d"         #'macrursors-mark-all-instances-of
;;        :desc "Mark all DWIM"          "D"         #'macrursors-mark-all-lines-or-instances
;;        ;;; by instance
;;        :desc "Mark next instance"     "f"         #'macrursors-mark-next-instance-of
;;        :desc "Mark previous instance" "b"         #'macrursors-mark-previous-instance-of
;;        ;;; by isearch
;;        :desc "Mark all isearch"       "s"         #'macrursors-mark-from-isearch
;;        :desc "Mark next isearch"      "s"         #'macrursors-mark-next-from-isearch
;;        :desc "Mark previous isearch"  "r"         #'macrursors-mark-previous-from-isearch
;;        ;;; by line
;;        :desc "Mark next line"         "n"         #'macrursors-mark-next-line
;;        :desc "Mark previous line"     "p"         #'macrursors-mark-previous-line
;;        ;;; apply/quit
;;        :desc "Apply changes"          "a"         #'macrursors-end
;;        :desc "Unmark all"             "q"         #'macrursors-early-quit))

;; (define-key macrursors-mark-map (kbd "C-;") #'macrursors-mark-all-lines-or-instances)
;; (define-key macrursors-mark-map (kbd ";") #'macrursors-mark-all-lines-or-instances)
;; (define-key macrursors-mark-map (kbd "l") #'macrursors-mark-all-lists)
;; (define-key macrursors-mark-map (kbd "s") #'macrursors-mark-all-symbols)
;; (define-key macrursors-mark-map (kbd "e") #'macrursors-mark-all-sexps)
;; (define-key macrursors-mark-map (kbd "f") #'macrursors-mark-all-defuns)
;; (define-key macrursors-mark-map (kbd "n") #'macrursors-mark-all-numbers)
;; (define-key macrursors-mark-map (kbd ".") #'macrursors-mark-all-sentences)
;; (define-key macrursors-mark-map (kbd "r") #'macrursors-mark-all-lines)
