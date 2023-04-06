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

(use-package! ryo-modal :demand t
  :init
  (define-global-minor-mode ryo-global-mode ryo-modal-mode
    (lambda ()
      (unless (minibufferp)
        (ryo-modal-mode 1))))
  (ryo-global-mode)
  :config
  (setq ryo-modal-cursor-type 'block)
  ;;; BINDINGS
  ;;; ========
  (map!
    ("<escape>" #'ryo-modal-mode)
    ;;; WINDOW
    ;;; ------
    ("C-H" #'shrink-window-horizontally)
    ("C-J" #'shrink-window)
    ("C-K" #'enlarge-window)
    ("C-L" #'enlarge-window-horizontally))
  ;;; ----
  (ryo-modal-keys (:norepeat t)
    ("C-@" "M-x")   ; M-x with C-SPC *ctrl
    ("C-SPC" "M-x") ; M-x with C-SPC *ctrl
    ;;; REPEAT
    ;;; ------
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
  ;;; ---
  (ryo-modal-keys
    ("<escape>" ignore) ; toggle ryo in minibuffer
    ;;; MOVE
    ;;; ----
    ("h" backward-char) ; ←
    ("j" next-line)     ; ↓
    ("k" previous-line) ; ↑
    ("l" forward-char)  ; →
    ;; ---
    ("H" my/back-to-indentation-or-bol) ; home      *shift
    ("J" scroll-down-command)           ; page up   *shift
    ("K" scroll-up-command)             ; page down *shift
    ("L" end-of-line)                   ; end       *shift
    ;; ---
    ("b" backward-word) ; ← word
    ("w" forward-word)  ; → word
    ;; ---
    ("B" sp-backward-symbol) ; ← symbol *shift
    ("W" sp-forward-symbol)  ; → symbol *shift
    ;; ---
    (":" my/exchange-point-and-pairs) ; toggle matching pairs *shift
    ;; ---
    ("o" sp-forward-sexp)          ; next sexp
    ("-" (("o" sp-backward-sexp))) ; previous sexp
    ;; ---
    ("O" sp-down-sexp)                ; next sexp down   *shift
    ("-" (("O" sp-backward-up-sexp))) ; previous sexp up *shift
    ;; ---
    ("[" (("w" backward-word)
          ("l" sp-backward-symbol)
          ("e" backward-sentence)
          ("p" backward-paragraph)
          ("b" beginning-of-buffer)
          ;; ("q" my/backward-quotes) ; TODO: implement?
          ("d" beginning-of-defun)
          ("r" my/backward-round)
          ("c" my/backward-curly)
          ("s" my/backward-square)
          ("a" my/backward-angle)))
    ;; ---
    ("]" (("w" forward-word)
          ("l" sp-forward-symbol)
          ("e" forward-sentence)
          ("p" forward-paragraph)
          ("b" end-of-buffer)
          ;; ("q" my/backward-quotes) ; TODO: implement?
          ("d" end-of-defun)
          ("r" my/forward-round)
          ("c" my/forward-curly)
          ("s" my/forward-square)
          ("a" my/forward-angle)))
    ;; ---
    ("n" my/char-forward :norepeat t)          ; next char     *repeat
    ("-" (("n" my/char-backward :norepeat t))) ; previous char *repeat
    ;; ---
    ("N" avy-goto-char :norepeat t)  ; goto char *shift
    ;;; isearch
    ;;; -------
    ("f" my/isearch-forward-region)          ; search-region-forward
    ("-" (("f" my/isearch-backward-region))) ; search-region-backward
    ;; ---
    ("F" isearch-query-replace :norepeat t) ; search and replace *shift
    ("-" (("F" isearch-exit :norepeat t)))  ; cancel search      *shift
    ;;; MARK
    ;;; ----
    ("SPC" set-mark-command)                   ; start mark
    ("-" (("SPC" my/deactivate-mark-command))) ; deactivate mark
    ;; ---
    ("v" my/expand-region)           ; expand region
    ("-" (("v" my/contract-region))) ; contract region
    ;; ---
    (";" exchange-point-and-mark) ; flip mark direction
    ;; ---
    ("s" my/mark-line-forward)          ; select line down
    ("-" (("s" my/mark-line-backward))) ; select line up
    ;; ---
    ("," (("q" my/mark-inside-quotes)
          ("d" my/mark-inside-defun)
          ("r" my/mark-inside-round)
          ("c" my/mark-inside-curly)
          ("s" my/mark-inside-square)))
    ;; ---
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
    ;;; GRAB
    ;;; ----
    ("g" my/unmark-and-cancel-grab) ; unselect all
    ("G" my/grab-region)            ; new grab / cancel grab *shift
    ("-" (("G" my/cancel-grab)))    ; cancel grab            *shift
    ;; ---
    ("r" my/swap-grab)                   ; swap grab
    ("-" (("r" my/sync-grab-content)))   ; sync grab content   *shift
    ;; ---
    ("R" my/swap-grab-content)           ; swap grab content *shift
    ("-" (("R" my/sync-region-content))) ; sync region content *shift
    ;;; EXIT
    ;;; ----
    ("i" my/beginning-of-region :exit t) ; insert before region
    ("a" my/end-of-region :exit t)       ; insert after region
    ;; ---
    ("I" my/open-line-up :exit t)   ; insert line up   *shift
    ("A" my/open-line-down :exit t) ; insert line down *shift
    ;;; ACTION
    ;;; ------
    ("x" my/kill-region-or-backward-char) ; cut
    ("y" kill-ring-save)                  ; copy
    ("p" yank)                            ; paste
    ("d" delete-char)                     ; delete
    ("\\" join-line)                      ; join line *deprecate
    ;; ---
    ("u" undo)              ; undo
    ("-" (("u" undo-redo))) ; redo
    ;; ---
    ("U" point-undo)         ; undo movement
    ("-" (("U" point-redo))) ; redo movement
    ;;; MISC
    ;;; ----
    ("m" my/start-or-cancel-macro :norepeat t) ; start/cancel macro
    ("M" my/start-or-save-macro :norepeat t)   ; save/execute macro  *shift
    ("e" my/execute-macro :norepeat t)         ; save/execute macro  *repeat
    ;; recenter buffer
    ("c" recenter :norepeat t))
  ;;; UNDERSCORE
  ;;; ==========
  (ryo-modal-keys
    ("_" (("o" sp-backward-sexp)
          ("O" sp-backward-up-sexp)
          ("n" my/char-backward :norepeat t)
          ("f" my/isearch-backward-region)
          ("F" isearch-exit :norepeat t)
          ("SPC" my/deactivate-mark-command)
          ("v" my/contract-region)
          ("s" my/mark-line-backward)
          ("G" my/cancel-grab)
          ("r" my/sync-grab-content)
          ("R" my/sync-region-content)
          ("u" undo-redo)
          ("U" point-redo))))
    ;;; FALLBACK
    ;;; ========
    (map! :leader
      ("j" "H-j")
      ("k" "H-k"))
    ;;; ISEARCH
    ;;; =======
    (ryo-modal-keys (:mode 'isearch-mode)
      ("TAB" isearch-complete-edit)
      ("f" isearch-forward-exit-minibuffer)
      ("-" (("r" isearch-reverse-exit-minibuffer)))
      ("p" isearch-yank-char-in-minibuffer))
    ;; ---
    (ryo-modal-keys
      ("C-f" isearch-forward)  ; search-forward  *ctrl
      ("C-r" isearch-backward) ; search-backward *ctrl
      ;; ---
      ("C-M-s" isearch-forward-regexp)    ; search-forward-regexp  *ctrl *alt
      ("C-M-r" isearch-backward-regexp))) ; search-backward-regexp *ctrl *alt
