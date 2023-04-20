;;; -*- lexical-binding: t; -*-

(setq my/lighter-theme-time "8:00")
(setq my/darker-theme-time "17:00")

(defun my/lighter-theme ()
  (setq doom-theme 'doom-gruvbox
        doom-gruvbox-dark-variant "medium"))

(defun my/darker-theme ()
  (setq doom-theme 'doom-gruvbox
        doom-gruvbox-dark-variant "hard"))

(defun my/autodetect-theme ()
  (let ((now (circadian-now-time)))
    (if (and (circadian-a-earlier-b-p (circadian-now-time) my/darker-theme-time)
            (circadian-a-earlier-b-p (circadian-now-time) my/lighter-theme-time))
      (my/lighter-theme)
      (my/darker-theme))))

(use-package circadian
  :ensure t
  :hook (circadian-before-load-theme-hook . my/autodetect-theme)
  :config
  (setq circadian-themes `((,my/lighter-theme-time . doom-gruvbox)
                           (,my/darker-theme-time . doom-gruvbox)))
  (circadian-setup))
