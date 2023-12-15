;;; -*- lexical-binding: t; -*-

(use-package consult
  :bind (([remap switch-to-buffer] . consult-buffer)
         ([remap bookmark-jump] . consult-bookmark)
         ([remap goto-line] . consult-goto-line)
         ([remap imenu] . consult-imenu)
         ([remap Info-search] . consult-info)
         ([remap list-directory] . consult-dir)
         ([remap load-theme] . consult-theme)
         ([remap locate] . consult-locate)
         ([remap find-file] . consult-find)
         ([remap man] . consult-man)
         ([remap recentf-open-files] . consult-recent-file)
         ([remap switch-to-buffer-other-frame] . consult-buffer-other-frame)
         ([remap switch-to-buffer-other-window] . consult-buffer-other-window)
         ([remap switch-to-buffer] . consult-buffer)
         ([remap yank-pop] . consult-yank-pop)))
;;  :map eglot-mode-map
;;  ([remap xref-find-apropos] . consult-eglot-symbols))
;; consult-lsp-symbols
