(use-package! ryo-modal)

(defvar saved-kmacro nil)

(defun start-or-cancel-kmacro ()
  (interactive)
  (if defining-kbd-macro
      (setq defining-kbd-macro nil)
    (start-kbd-macro nil)))

(defun resume-or-save-kmacro ()
  (interactive)
  (if defining-kbd-macro
      (progn
        (end-kbd-macro)
        (setq saved-kmacro last-kbd-macro))
    (setq defining-kbd-macro t)))

(defun execute-saved-kmacro ()
  (interactive)
  (if (null saved-kmacro)
      (message "No saved macro.")
    (let ((saved-prefix last-prefix-arg))
      (ignore-errors (execute-kbd-macro saved-kmacro))
      (setq ryo-modal--last-command-prefix-arg saved-prefix)
      (setq ryo-modal--last-command #'execute-saved-kmacro)
      (message "Executed saved macro."))))

(defun execute-kmacro ()
  (interactive)
  (let ((saved-prefix last-prefix-arg))
    (ignore-errors (kmacro-call-macro))
    (setq ryo-modal--last-command-prefix-arg saved-prefix)
    (setq ryo-modal--last-command #'execute-kmacro)
    (message "Executed saved macro.")))

(provide 'save-kmacro)
