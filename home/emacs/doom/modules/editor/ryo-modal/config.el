;;; evil-core
(require 'evil-core)

(evil-esc-mode 1)

;;; smartparens
(use-package! smartparens)

;;; point-undo
(use-package! point-undo
  :demand t
  :init)

;;;
;;; custom packages
;;;

(use-package! char-occurrence)
(use-package! open-line)
(use-package! region-boundaries)
(use-package! repeat-nth :demand t)
(use-package! save-kmacro)
(use-package! save-point)
(use-package! secondary-selection)
(use-package! smarter-commands)
(use-package! text-object)

;;;
;;; modeline
;;;

(defun doom-modeline--evil-ryo ()
  "The current ryo-modal state which is enabled by the command `ryo-modal-mode'."
  (cond 
    ;; visual
    ((and ryo-modal-mode (use-region-p))
      (doom-modeline--modal-icon "VISUAL" 'doom-modeline-evil-visual-state "Ryo modal" "add_circle" "✪"))
    ;; normal
    (ryo-modal-mode
      (doom-modeline--modal-icon "NORMAL" 'doom-modeline-evil-normal-state "Ryo modal" "add_circle" "✪"))
    ;; insert
    (t
      (doom-modeline--modal-icon "INSERT" 'doom-modeline-evil-insert-state "Holy mode"))))

(use-package! doom-modeline
  :config
  (doom-modeline-def-segment modals
    (when doom-modeline-modal
      (concat
        (doom-modeline-spc)
        (doom-modeline--evil-ryo)
        (doom-modeline-spc)))))

(use-package! which-key
  :config
  (push '((nil . "ryo:.*:") . (nil . "")) which-key-replacement-alist))

;;;
;;; bindings
;;;

(use-package! ryo-modal
  :demand t
  :bind
  ("<escape>" . ryo-modal-mode)
  :config
  (setq-default cursor-type 'bar)
  (setq-default blink-cursor-blinks 0)
  (setq ryo-modal-cursor-type 'block)
  (ryo-modal-keys
    ("<escape>" keyboard-escape-quit)
    ("0" repeat-ten :norepeat t)
    ("1" ryo-modal-repeat :norepeat t)
    ("2" repeat-two :norepeat t)
    ("3" repeat-three :norepeat t)
    ("4" repeat-four :norepeat t)
    ("5" repeat-five :norepeat t)
    ("6" repeat-six :norepeat t)
    ("7" repeat-seven :norepeat t)
    ("8" repeat-eight :norepeat t)
    ("9" repeat-nine :norepeat t)
    ;;; ergonomics
    ("C-@" "M-x" :norepeat t)
    ("C-SPC" "M-x" :norepeat t)
    ;;; movement
    ("l" forward-char)
    ("L" end-of-line)
    ("h" backward-char)
    ("H" back-to-indentation-or-beginning-of-line)
    ("j" next-line)
    ("J" scroll-down-command)
    ("k" previous-line)
    ("K" scroll-up-command)
    ("w" forward-word)
    ("W" sp-forward-symbol)
    ("b" backward-word)
    ("B" sp-backward-symbol)
    ("o" match-sexp)
    ("O" sp-beginning-of-next-sexp)
    ("]" (("p" forward-paragraph)
          ("s" forward-sentence)
          ("d" end-of-defun)
          ("b" end-of-buffer)))
    ("[" (("p" backward-paragraph)
          ("s" backward-sentence)
          ("d" beginning-of-defun)
          ("b" beginning-of-buffer)))
    ("t" go-to-char-forward :norepeat t) ; internal repeat
    ("T" avy-goto-char :norepeat t)
    ("f" isearch-region-forward) ; internal repeat
    ("n" isearch-repeat-forward-region :norepeat t) ; internal repeat
    ("N" isearch-exit :norepeat t)
    ("." restore-point :norepeat t)
    ("z" point-undo)
    ;;; mark
    ("v" toggle-mark)
    ("V" set-mark-sexp :norepeat t)
    ("g" "C-g")
    ("G" grab-region)
    (";" exchange-point-and-mark)
    ("s" mark-whole-line-to-next)
    ;;; manipulation
    ("a" go-to-end-of-region :exit t :norepeat t)
    ("A" open-line-down :exit t :norepeat t)
    ("i" go-to-beginning-of-region :exit t :norepeat t)
    ("I" open-line-up :exit t :norepeat t)
    ("d" delete-forward-char-or-region)
    ("c" delete-char :exit t :norepeat t)
    ("C" recenter :norepeat t)
    ("x" delete-backward-char-or-kill-region)
    ("y" kill-ring-save)
    ("p" yank)
    ("," join-line)
    ("u" undo)
    ("r" swap-grab)
    ("R" sync-grab-content :norepeat t)
    ;;; macros
    ("m" start-or-cancel-macro :norepeat t)
    ("M" start-or-save-macro :norepeat t)
    ("e" execute-macro :norepeat t) ; internal repeat
    ;;; reverse
    ("-" (("O" sp-beginning-of-previous-sexp)
          ("t" go-to-char-backward :norepeat t) ; internal repeat
          ("f" isearch-region-backward :norepeat t) ; internal repeat
          ("n" isearch-repeat-backward-region :norepeat t) ; internal repeat
          ("." save-point :norepeat t)
          ("z" point-redo)
          ("s" mark-whole-line-to-previous)
          ("u" undo-redo)
          ("r" swap-grab-content)
          ("R" sync-region-content :norepeat t)
          ("M" kmacro-cycle-ring-next :norepeat t)
          ("M" kmacro-cycle-ring-previous :norepeat t))))
  (map! :map ryo-modal-mode-map
    doom-leader-key doom-leader-map))
