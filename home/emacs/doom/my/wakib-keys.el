;;; -*- lexical-binding: t; -*-

(load! "evil-core")

(evil-esc-mode 1)

;;; point-undo

(use-package! point-undo
  :demand t
  :init)

;;; which-key

(use-package! which-key
  :config
  (push '((nil . "ryo:.*:") . (nil . "")) which-key-replacement-alist)
  :init
  (which-key-mode))

(use-package! expand-region
  :custom
  (expand-region-fast-keys-enabled nil))

(use-package! smartparens)

(load! "grab")
(load! "macro")
(load! "search")

;;; wakib-keys

(defun my/kill-region-or-whole-line (arg)
  (interactive "p")
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (kill-whole-line arg)))

(use-package! wakib-keys
  :init
  (wakib-keys 1)
  :config
  (setq wakib-keylist
        `(("<escape>" . keyboard-quit)
          ("M-<f4>" . save-buffers-kill-emacs)
          ("S-<f9>" . treemacs-select-window)
          ;;
          ("<left>" . left-char)
          ("<down>" . next-line)
          ("<up>" . previous-line)
          ("<right>" . right-char)
          ("C-<left>" . backward-word)
          ("C-<down>" . scroll-down-line)
          ("C-<up>" . scroll-up-line)
          ("C-<right>" . forward-word)
          ("M-<left>" . point-undo)
          ("M-<down>" . wakib-previous)
          ("M-<up>" . wakib-next)
          ("M-<right>" . point-redo)
          ;;
          ("<backspace>" . delete-backward-char)
          ("C-<backspace>" . backward-kill-word)
          ("<C-return>" . wakib-insert-line-after)
          ("<C-S-return>" . wakib-insert-line-before)
          ("M-<return>" . avy-goto-char)
          ("<delete>" . delete-char)
          ("C-<delete>" . kill-word)
          ("<home>" . wakib-back-to-indentation-or-beginning)
          ("<end>" . move-end-of-line)
          ("<next>" . scroll-down-command)
          ("<prior>" . scroll-up-command)
          ("C-<next>" . next-buffer)
          ("C-<prior>" . previous-buffer)
          ;;
          ("C-'" . ace-window)
          ("C-1" . delete-other-windows)
          ("C-2" . split-window-below)
          ("C-3" . split-window-right)
          ("C-0" . delete-window)
          ("C-=" . text-scale-increase)
          ("C--" . text-scale-decrease)
          ("C-+" . text-scale-increase)
          ;; ("g" my/unmark-and-cancel-grab)
          ("C-q" . save-buffers-kill-terminal)
          ("C-w" . kill-current-buffer)
          ("C-r" . my/start-or-cancel-macro)
          ("C-S-r" . my/start-or-save-macro)
          ("M-r" . my/execute-macro)
          ("C-h" . isearch-query-replace) ; query-replace
          ("C-S-h" . isearch-query-replace-regexp) ; query-replace-regexp
          ("C-p" . find-file)
          ("C-S-p" . execute-extended-command)
          ("M-p" . switch-to-buffer)
          ;;
          ("C-a" . mark-whole-buffer)
          ("C-s" . save-buffer)
          ("C-S-s" . write-file)
          ("C-f" . my/isearch-forward-region)
          ("C-S-f" . my/isearch-backward-region)
          ("M-f" . isearch-forward-regexp)
          ("M-S-f" . isearch-backward-regexp)
          ("M-S-g" . goto-line)
          ;;
          ("C-z" . undo)
          ("C-S-z" . undo-redo)
          ("C-x" . my/kill-region-or-whole-line)
          ("M-S-x" . pp-eval-expression)
          ("C-c" . kill-ring-save)
          ("M-c" . recenter)
          ("C-v" . yank)
          ("C-b" . +treemacs/toggle)
          ("M-b" . treemacs-select-window)
          ("C-n" . wakib-new-empty-buffer)
          ("C-m" my/sync-grab-content)
          ("C-S-m" my/cancel-grab)
          ("M-m" my/swap-grab)
          ("M-S-m" . my/swap-grab-content)
          ("C-;" . comment-line)))
  (wakib-define-keys wakib-keys-overriding-map wakib-keylist))
