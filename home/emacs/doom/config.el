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

(use-package! which-key
  :config
  (setq which-key-show-early-on-C-h t)
  (setq which-key-idle-delay 0.05)
  (setq which-key-idle-secondary-delay 0.05)
  (setq which-key-popup-type 'side-window)
  (setq which-key-side-window-location 'right))

(use-package! all-the-icons-nerd-fonts
  :after all-the-icons
  :demand t
  :config
  (all-the-icons-nerd-fonts-prefer))

(use-package! dired-sidebar
  :commands (dired-sidebar-toggle-sidebar dired-sidebar-find-file)
  :bind
  ("<f9>" . dired-sidebar-toggle-sidebar)
  ("<C-f9>" . dired-sidebar-find-file)
  :config
  (setq dired-sidebar-should-follow-file t)
  (setq dired-sidebar-recenter-cursor-on-tui-update t)
  (setq dired-sidebar-use-one-instance t))

(map! :leader
  (:prefix-map ("o" . "open")
    :desc "Project sidebar"               "p" #'dired-sidebar-toggle-sidebar
    :desc "Find file in project rsidebar" "P" #'dired-sidebar-find-file))

(defun my/dashboard-draw-ascii-banner-fn ()
  (let* ((banner
          '("                                                         "
            " ______     __    __     ______     ______     ______    "
            "/\\  ___\\   /\\ \"-./  \\   /\\  __ \\   /\\  ___\\   /\\  ___\\   "
            "\\ \\  __\\   \\ \\ \\-./\\ \\  \\ \\  __ \\  \\ \\ \\____  \\ \\___  \\  "
            " \\ \\_____\\  \\ \\_\\ \\ \\_\\  \\ \\_\\ \\_\\  \\ \\_____\\  \\/\\_____\\ "
            "  \\/_____/   \\/_/  \\/_/   \\/_/\\/_/   \\/_____/   \\/_____/ "
            "                                                         "))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; fix mouse scroll wheel
(map! :unless (display-graphic-p)
  "<mouse-4>" (cmd! (scroll-down 2))
  "<mouse-5>" (cmd! (scroll-up 2)))

(setq completion-styles '(orderless)
      ;; orderless-matching-styles '(orderless-flex orderless-literal orderless-regexp)
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
