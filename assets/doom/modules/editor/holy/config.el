;;; editor/holy/config.el -*- lexical-binding: t; -*-

(setq doom-leader-key "C-c"
      doom-leader-alt-key "C-c"
      doom-localleader-key "C-c l"
      doom-localleader-alt-key "C-c l")

(defun disable-meow ()
  (meow-global-mode -1))

(use-package! meow :demand t
  :hook (meow-normal-mode-hook . #'disable-meow)
  :config
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty
        meow-cursor-type-default '(hbar . 4)
        meow-cursor-type-normal '(hbar . 4)
        meow-cursor-type-motion '(hbar . 4)
        meow-cursor-type-beacon '(hbar . 4)
        meow-cursor-type-insert '(bar . 4)
        meow-cursor-type-region-cursor '(bar . 4)
        meow-cursor-type-keypad 'hollow
        meow-keypad-leader-dispatch ctl-x-map
        meow-keypad-start-keys '((?c . ?c)
                                 (?h . ?h)
                                 (?x . ?x))))

;; (defun my/comment ()
;;   (interactive)
;;   (if (use-region-p)
;;       (comment-or-uncomment-region (region-beginning) (region-end))
;;     (comment-or-uncomment-region (line-beginning-position) (line-end-position)))
;;   (setq deactivate-mark nil))

(map!
  "C-SPC" 'meow-keypad)
