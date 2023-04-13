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

;;; doom-modeline

(use-package! doom-modeline
  :config
  (defun doom-modeline--evil-ryo ()
    "The current ryo-modal state which is enabled by the command `ryo-modal-mode'."
    (cond
      ;; normal
      (ryo-modal-mode
        (doom-modeline--modal-icon "VSCODE" 'doom-modeline-evil-normal-state "Ryo modal" "add_circle" "✪"))
      ;; insert
      (t
        (doom-modeline--modal-icon "EMACS" 'doom-modeline-evil-insert-state "Holy mode"))))
  (doom-modeline-def-segment modals
    (when doom-modeline-modal
      (concat
        (doom-modeline-spc)
        (doom-modeline--evil-ryo)
        (doom-modeline-spc)))))

(use-package! expand-region
  :custom
  (expand-region-fast-keys-enabled nil))

(use-package! point-undo)
(use-package! smartparens)

;;; ryo-modal

;; (load! "exit")
;; (load! "grab")
;; (load! "macro")
(load! "mark")
;; (load! "move")
;; (load! "perform")
;; (load! "repeat")
;; (load! "search")

(use-package! ryo-modal :demand t
  :init
  (define-global-minor-mode ryo-global-mode ryo-modal-mode
    (lambda ()
      (unless (minibufferp)
        (ryo-modal-mode 1))))
  (ryo-global-mode)
  :config
  ;;; BINDINGS
  ;;; ========
  (map!
    ("<escape>" #'ryo-modal-mode))
  ;;; ---
  (ryo-modal-keys (:norepeat t)
    ("<escape>" ignore) ; toggle ryo in minibuffer

    ("<left>" backward-char :first '(my/deactivate-mark-if-active))
    ("<right>" forward-char :first '(my/deactivate-mark-if-active))
    ("<up>" previous-line :first '(my/deactivate-mark-if-active))
    ("<down>" next-line :first '(my/deactivate-mark-if-active))
    
    ("<S-left>" backward-char :first '(my/set-mark-if-inactive))
    ("<S-right>" forward-char :first '(my/set-mark-if-inactive))
    ("<S-up>" previous-line :first '(my/set-mark-if-inactive))
    ("<S-down>" next-line :first '(my/set-mark-if-inactive))
    
    ("<C-left>" backward-word :first '(my/deactivate-mark-if-active))
    ("<C-right>" forward-word :first '(my/deactivate-mark-if-active))
    ("<C-up>" scroll-down-line)
    ("<C-down>" scroll-up-line)
    
    ("<C-S-left>" backward-word :first '(my/set-mark-if-inactive))
    ("<C-S-right>" forward-word :first '(my/set-mark-if-inactive))
    ("<C-S-up>" previous-line :first '(my/set-mark-if-inactive))
    ("<C-S-down>" next-line :first '(my/set-mark-if-inactive))

    ("C-S-P" execute-extended-command)
    ("C-p" find-file)

    ("C-S-Z" undo-redo)
    ("C-z" undo)

    
    ))
