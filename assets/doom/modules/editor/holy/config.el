;;; editor/holy/config.el -*- lexical-binding: t; -*-

(setq doom-leader-key "C-c"
      doom-leader-alt-key "C-c"
      doom-localleader-key "C-c SPC"
      doom-localleader-alt-key "C-c SPC")

;; (defun my/comment ()
;;   (interactive)
;;   (if (use-region-p)
;;       (comment-or-uncomment-region (region-beginning) (region-end))
;;     (comment-or-uncomment-region (line-beginning-position) (line-end-position)))
;;   (setq deactivate-mark nil))
