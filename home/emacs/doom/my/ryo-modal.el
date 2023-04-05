;;; -*- lexical-binding: t; -*-

(load! "evil-core")

(evil-esc-mode 1)

;;; undo-tree

(use-package! undo-tree
  :demand t
  :init
  (undo-tree-mode t))

;;; point-undo

(use-package! point-undo
  :demand t
  :init)

;;; which-key

(use-package! which-key
  :config
  (push '((nil . "ryo:.*:") . (nil . "")) which-key-replacement-alist))

;;; doom-modeline

(use-package! doom-modeline
  :config
  (defun doom-modeline--evil-ryo ()
    "The current ryo-modal state which is enabled by the command `ryo-modal-mode'."
    (cond 
      ;; visual
      ((and ryo-modal-mode mark-active)
        (doom-modeline--modal-icon "VISUAL" 'doom-modeline-evil-visual-state "Ryo modal" "add_circle" "✪"))
      ;; normal
      (ryo-modal-mode
        (doom-modeline--modal-icon "NORMAL" 'doom-modeline-evil-normal-state "Ryo modal" "add_circle" "✪"))
      ;; insert
      (t
        (doom-modeline--modal-icon "INSERT" 'doom-modeline-evil-insert-state "Holy mode"))))
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

(load! "exit")
(load! "grab")
(load! "macro")
(load! "mark")
(load! "move")
(load! "perform")
(load! "repeat")
(load! "search")

(use-package! ryo-modal
  :bind ("<escape>" . ryo-modal-mode)
  :hook ((text-mode prog-mode isearch-mode) . ryo-modal-mode)
  :demand t
  :config
  (setq-default cursor-type 'bar
                blink-cursor-blinks 0)
  (setq ryo-modal-cursor-type 'block)
  ;;---
  (ryo-modal-keys
    (:norepeat t)

    ;;; ---
    ;;; BASE
    ;;; ---

    ("<escape>" "C-g") ; cancel
    ("C-@" "M-x")      ; *ctrl
    ("C-SPC" "M-x")    ; *ctrl

    ;;; ---
    ;;; REPEAT
    ;;; ---

    ("1" ryo-modal-repeat)
    ("2" my/ryo-modal-repeat-two)
    ("3" my/ryo-modal-repeat-three)
    ("4" my/ryo-modal-repeat-four)
    ("5" my/ryo-modal-repeat-five)
    ("6" my/ryo-modal-repeat-six)
    ("7" my/ryo-modal-repeat-seven)
    ("8" my/ryo-modal-repeat-eight)
    ("9" my/ryo-modal-repeat-nine)
    ("0" my/ryo-modal-repeat-ten))
  ;;---
  (ryo-modal-keys

    ;;; ---
    ;;; MOVE
    ;;; ---

    ;; char

    ("h" backward-char) ; ↓
    ("j" next-line)     ; ↑
    ("k" previous-line) ; →
    ("l" forward-char)  ; ←

    ;; extreme

    ("H" my/back-to-indentation-or-bol) ; home      *shift
    ("J" scroll-down-command)           ; page up   *shift
    ("K" scroll-up-command)             ; page down *shift
    ("L" end-of-line)                   ; end       *shift

    ;; word

    ("w" backward-word) ; ← word
    ("e" forward-word)  ; → word

    ;; symbol

    ("W" sp-backward-symbol) ; ← symbol *shift
    ("E" sp-forward-symbol)  ; → symbol *shift

    ;; pairs
    
    (":" my/exchange-point-and-pairs) ; toggle matching pairs *shift
    
    ;; sexp

    ("o" sp-forward-sexp)          ; next sexp
    ("-" (("o" sp-backward-sexp))) ; previous sexp

    ;; depth
    ("O" sp-down-sexp)                ; next sexp down   *shift
    ("-" (("O" sp-backward-up-sexp))) ; previous sexp up *shift

    ("[" (("w" backward-word)
          ("l" sp-backward-symbol)
          ("e" backward-sentence)
          ("p" backward-paragraph)
          ("b" beginning-of-buffer)
          ;; ("q" my/backward-quotes) ; TODO: implement
          ("d" beginning-of-defun)
          ("r" my/backward-round)
          ("c" my/backward-curly)
          ("s" my/backward-square)))

    ("]" (("w" forward-word)
          ("l" sp-forward-symbol)
          ("e" forward-sentence)
          ("p" forward-paragraph)
          ("b" end-of-buffer)
          ;; ("q" my/backward-quotes) ; TODO: implement
          ("d" end-of-defun)
          ("r" my/forward-round)
          ("c" my/forward-curly)
          ("s" my/forward-square)))

    ("n" my/char-forward :norepeat t)          ; next char     *repeat
    ("-" (("n" my/char-backward :norepeat t))) ; previous char *repeat

    ("N" avy-goto-char :norepeat t)  ; goto char

    ;;; ---
    ;;; isearch
    ;;; ---

    ("C-f" isearch-forward)  ; search-forward  *ctrl
    ("C-r" isearch-backward) ; search-backward *ctrl

    ("C-M-s" isearch-forward-regexp)  ; search-forward-regexp  *ctrl *alt
    ("C-M-r" isearch-backward-regexp) ; search-backward-regexp *ctrl *alt

    ("f" my/isearch-forward-region)          ; search-region-forward
    ("-" (("f" my/isearch-backward-region))) ; search-region-backward

    ("F" isearch-query-replace :norepeat t) ; search and replace
    ("-" (("F" isearch-exit :norepeat t)))    ; cancel search

    ;;; ---
    ;;; MARK
    ;;; ---

    ("v" set-mark-command)                   ; start mark
    ("-" (("v" my/deactivate-mark-command))) ; deactivate mark

    ("V" my/expand-region)           ; expand region   *shift
    ("-" (("V" my/contract-region))) ; contract region *shift

    (";" exchange-point-and-mark) ; flip mark direction
    
    ("s" my/mark-line-forward)           ; select line down
    ("-"  (("s" my/mark-line-backward))) ; select line up

    ("," (("q" my/mark-inside-quotes)
          ("d" my/mark-inside-defun)
          ("r" my/mark-inside-round)
          ("c" my/mark-inside-curly)
          ("s" my/mark-inside-square)))

    ("." (("w" my/mark-word)
          ("l" my/mark-symbol)
          ("e" my/mark-sentence)
          ("p" my/mark-paragraph)
          ("b" my/mark-whole-buffer)
          ("q" my/mark-outside-quotes)
          ("d" my/mark-outside-defun)
          ("r" my/mark-outside-round)
          ("c" my/mark-outside-curly)
          ("s" my/mark-outside-square)))
 
    ;;; ---
    ;;; GRAB
    ;;; ---

    ("g" my/unmark-and-cancel-grab) ; unselect all
    ("G" my/grab-region)            ; new grab / cancel grab *shift
    ("-" (("G" my/cancel-grab)))    ; cancel grab            *shift

    ("r" my/swap-grab-content) ; swap grab

    ("R" my/sync-grab-content)           ; sync grab content   *shift
    ("-" (("R" my/sync-region-content))) ; sync region content *shift

    ;;; ---
    ;;; EXIT
    ;;; ---

    ("i" my/beginning-of-region :exit t) ; insert before region
    ("a" my/end-of-region :exit t)       ; insert after region

    ("I" my/open-line-up :exit t)   ; insert line up   *shift
    ("A" my/open-line-down :exit t) ; insert line down *shift

    ;;; ---
    ;;; ACTION
    ;;; ---

    ;; basics

    ("x" my/kill-region-or-backward-char) ; cut
    ("y" kill-ring-save)                  ; copy
    ("p" yank)                            ; paste
    ("d" delete-char)                     ; delete
    ("\\" join-line)                      ; join line ; TODO: deprecate?

    ;; undo/redo

    ("u" undo-tree-undo)         ; undo
    ("-" (("u" undo-tree-redo))) ; redo

    ("z" point-undo)         ; undo movement
    ("-" (("z" point-redo))) ; redo movement

    ;;; ---
    ;;; MISC
    ;;; ---

    ("m" my/start-or-cancel-macro :norepeat t) ; record/cancel macro
    ("M" my/save-or-execute-macro :norepeat t) ; save/execute macro  *shift *repeat

    ; recenter buffer
    ("c" recenter :norepeat t)))
