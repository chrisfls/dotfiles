;; -*- lexical-binding: t; -*-

(defun my/font-size (n)
  (truncate (* n 10 scale)))

(set-face-attribute 'default nil
                    :family "JetBrains Mono NFM"
                    :height (my/font-size 12.0))

(set-face-attribute 'variable-pitch nil
                    :family "Noto Sans"
                    :height (my/font-size 13.0))

(set-face-attribute 'fixed-pitch nil
                    :family "Noto Sans Mono"
                    :height (my/font-size 13.0))
