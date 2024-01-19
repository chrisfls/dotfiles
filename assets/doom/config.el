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
      doom-leader-key "\\"
      doom-leader-alt-key "C-\\"
      doom-localleader-key "|"
      doom-localleader-alt-key "C-|"
      +doom-dashboard-ascii-banner-fn nil
      +word-wrap-extra-indent 'single)

(setq display-line-numbers-type 'relative
      mouse-wheel-follow-mouse 't
      mouse-wheel-progressive-speed nil
      mouse-wheel-scroll-amount '(1 ((shift) . 1))
      ring-bell-function 'ignore
      scroll-margin 3
      scroll-step 1
      scroll-step 3)

(setq-default cursor-type '(bar . 4)
              display-fill-column-indicator-column 79
              display-fill-column-indicator-character ?╎
              fill-column 80)

(global-display-fill-column-indicator-mode 1)
(pixel-scroll-precision-mode 1)
(+global-word-wrap-mode 1)
