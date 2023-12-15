
(setq-default
  avy-style 'de-bruijn
  completion-styles '(orderless)
  isearch-allow-scroll t
  isearch-lazy-highlight-initial-delay 0
  orderless-matching-styles '(orderless-flex orderless-literal orderless-regexp)
  read-file-name-completion-ignore-case t
  scroll-error-top-bottom t
  scroll-preserve-screen-position nil
  vertico-posframe-poshandler #'posframe-poshandler-frame-top-center
  left-margin-width 0
  cursor-type '(bar . 4)
  blink-cursor-blinks -1
  display-fill-column-indicator-column 79)

(+global-word-wrap-mode +1)
(set-face-background 'cursor "white")
(blink-cursor-mode +1)
