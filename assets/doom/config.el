;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq-default cursor-type '(bar . 4)
              display-fill-column-indicator-column 79
              display-fill-column-indicator-character ?\s
              fill-column 80
              mode-line-format nil)

;; TODO: doom-big-font doom-symbol-font doom-serif-font
(setq user-full-name "Christian Ferraz"
      user-mail-address ""
      org-directory "~/Desktop/org/"
      doom-font (font-spec :family "Jetbrains Mono NFM" :size (* 12.0 1.5)) ; :weight 'semi-light
      doom-variable-pitch-font (font-spec :family "Noto Sans" :size (* 13.0 1.5))
      doom-theme 'doom-outrun-electric ; 'doom-gruvbox 'doom-dracula 'doom-monokai-classic 'doom-tokyo-night
      doom-gruvbox-dark-variant "hard"
      +doom-dashboard-ascii-banner-fn nil
      +word-wrap-extra-indent 'single
      mouse-wheel-follow-mouse 't
      mouse-wheel-progressive-speed nil
      mouse-wheel-scroll-amount '(3 ((shift) . 3))
      ring-bell-function 'ignore
      blink-cursor-blinks 0
      scroll-conservatively 10000
      scroll-margin 3
      scroll-preserve-screen-position 1
      scroll-step 3
      mode-line-format nil)

(after! highlight-indent-guides
  (setq highlight-indent-guides-method 'column
        highlight-indent-guides-responsive 'top
        highlight-indent-guides-auto-odd-face-perc 25
        highlight-indent-guides-auto-even-face-perc 25
        highlight-indent-guides-auto-character-face-perc 50)
  (highlight-indent-guides-auto-set-faces)
  ;; TODO: support light themes
  (set-face-attribute 'fill-column-indicator nil
      :background (face-attribute 'highlight-indent-guides-even-face
                                  :background nil
                                  'default)
      :foreground nil))

(+global-word-wrap-mode 1)
(blink-cursor-mode 1)
(global-display-fill-column-indicator-mode 1)

(set-frame-parameter nil 'alpha-background 90)
(add-to-list 'default-frame-alist '(alpha-background . 90))

(after! recentf
 (add-to-list 'recentf-exclude "~/.config/emacs/.local"))

(after! vertico
  (setq vertico-count 10
        vertico-scroll-margin 0))

;; (use-package! vertico-buffer :demand t
;;   :init
;;   (vertico-buffer-mode 1)
;;   :config
;;   (setq vertico-buffer-display-action
;;       '(display-buffer-in-side-window
;;         (window-height . 12)
;;         (side . top))) )
