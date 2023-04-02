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
;;; bindings
;;;

(use-package! ryo-modal
  :demand t
  :bind
  ("<escape>" . ryo-modal-mode)
  :config
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
    ("f" isearch-forward-regexp)
    ("n" isearch-repeat-forward-regexp :norepeat t) ; internal repeat
    ("." save-point :norepeat t)
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
    ("x" delete-backward-char-or-kill-region)
    ("y" kill-ring-save)
    ("p" yank)
    ("," join-line)
    ("u" undo)
    ("r" swap-grab)
    ("R" sync-grab-content :norepeat t )
    ;;; macros
    ("m" toggle-recording-macro :norepeat t )
    ("M" start-or-save-macro :norepeat t )
    ("e" execute-macro :norepeat t) ; internal repeat
    ;;; reverse
    ("-" (("O" sp-beginning-of-previous-sexp)
          ("t" go-to-char-backward :norepeat t) ; internal repeat
          ("f" isearch-backward-regexp :norepeat t)
          ("n" isearch-repeat-backward-regexp :norepeat t) ; internal repeat
          ("." restore-point :norepeat t)
          ("z" point-redo)
          ("s" mark-whole-line-to-previous)
          ("u" undo-redo)
          ("r" swap-grab-content)
          ("R" sync-region-content :norepeat t)
          ("M" kmacro-cycle-ring-next :norepeat t)
          ("M" kmacro-cycle-ring-previous :norepeat t)))))
