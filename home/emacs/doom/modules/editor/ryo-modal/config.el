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
(use-package! repeat-nth)
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
    ("l" forward-char :name "forward char")
    ("L" end-of-line :norepeat t :name "end-of line")
    ("h" backward-char :name "backward char")
    ("H" back-to-indentation-or-beginning-of-line :name "beginning of line")
    ("j" next-line :name "next line")
    ("J" scroll-down-command :name "page down")
    ("k" previous-line :name "previous line")
    ("K" scroll-up-command :name "page up")
    ("w" forward-word :name "forward word")
    ("W" sp-forward-symbol :name "forward symbol")
    ("b" backward-word :name "backward word")
    ("B" sp-backward-symbol :name "backward symbol")
    ("o" match-sexp :name "match sexp")
    ("O" sp-beginning-of-next-sexp :name "beginning of next sexp")
    ("]" (("p" forward-paragraph :name "forward paragraph")
          ("s" forward-sentence :name "forward sentence")
          ("d" end-of-defun :name "end of defun")
          ("b" end-of-buffer :norepeat t :name "end of buffer")))
    ("[" (("p" backward-paragraph :name "backward paragraph")
          ("s" backward-sentence :name "backward sentence")
          ("d" beginning-of-defun :name "beginning of defun")
          ("b" beginning-of-buffer :norepeat t :name "beginning of buffer")))
    ("f" go-to-char-forward :norepeat t :name "go to char forward")
    ("F" avy-goto-char :norepeat t)
    ("t" go-till-char-forward :norepeat t :name "go till char forward")
    ("v" isearch-forward-regexp :norepeat t :name "search forward")
    ("n" isearch-repeat-forward :name "search repeat forward")
    ("." save-point :norepeat t :name "save point position")
    ("z" point-undo :norepeat t :name "backward point")
    ;;; mark
    ("SPC" set-mark-command :norepeat t :name "set mark")
    ("S-SPC" set-mark-sexp :norepeat t :name "set mark sexp")
    ("g" deactivate-mark-command :norepeat t :name "deactivate mark")
    ("G" grab-region :norepeat t :name "grab region")
    (";" exchange-point-and-mark :norepeat t :name "reverse point and mark")
    ("s" mark-whole-line-to-next :norepeat t :name "mark whole line to next")
    ;;; manipulation
    ("a" go-to-end-of-region :exit t :norepeat t :name "append")
    ("A" open-line-down :exit t :norepeat t :name "open down")
    ("i" go-to-beginning-of-region :exit t :norepeat t :name "insert")
    ("I" open-line-up :exit t :norepeat t :name "open down")
    ("d" delete-forward-char-or-region :name "delete")
    ("c" delete-char :exit t :norepeat t :name "change")
    ("x" delete-backward-char-or-kill-region :name "kill")
    ("y" kill-ring-save :norepeat t :name "save")
    ("p" yank :name "paste")
    ("," join-line :name "join line")
    ("u" undo :name "undo")
    ("r" swap-grab :name "swap grab")
    ("R" swap-grab-content :name "swap grab content")
    ("Y" sync-grab-content :norepeat t :name "sync grab content")
    ;;; macros
    ("m" start-or-save-kmacro :norepeat t :name "start or save kmacro")
    ("e" execute-saved-kmacro :name "execute saved kmacro")
    ("M" start-or-cancel-kmacro :norepeat t :name "start or cancel kmacro")
    ("E" kmacro-call-macro :name "apply last macro")
    ;;; reverse
    ("-" (("O" sp-beginning-of-previous-sexp :name "beginning of previous sexp")
          ("f" go-to-char-backward :norepeat t :name "go to char backward")
          ("t" go-till-char-backward :norepeat t :name "go till char backward")
          ("v" isearch-backward-regexp :norepeat t :name "search backward")
          ("n" isearch-repeat-backward :name "search repeat backward")
          ("." restore-point :norepeat t :name "restore point")
          ("z" point-redo :name "forward point")
          ("s" mark-whole-line-to-previous :name "mark whole line to previous")
          ("u" undo-redo :name "redo")
          ("Y" sync-region-content :norepeat t :name "sync region content")))))
