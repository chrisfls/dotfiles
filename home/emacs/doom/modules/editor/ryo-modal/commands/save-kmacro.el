(defun start-or-save-macro ()
  (interactive)
  (if defining-kbd-macro
      (progn
        (end-kbd-macro)
        (message "Saved macro recording."))
    (when last-kbd-macro
      (kmacro-push-ring))
    (start-kbd-macro nil)
    (message "Started macro recording.")))

(defun start-or-cancel-macro ()
  (interactive)
  (if defining-kbd-macro
      (let ((prev-kbd-macro last-kbd-macro))
        (end-kbd-macro)
        (setq last-kbd-macro prev-kbd-macro)
        (message "Cancelled macro recording."))
    (when last-kbd-macro
      (kmacro-push-ring))
    (start-kbd-macro nil)
    (message "Started macro recording.")))

(defun execute-macro ()
  (interactive)
  (let ((saved-prefix last-prefix-arg))
    (if (null last-kbd-macro)
        (error "No recorded macro.")
      (ignore-errors (execute-kbd-macro last-kbd-macro))
      (message "Executed macro."))
    (setq ryo-modal--last-command-prefix-arg saved-prefix)
    (setq ryo-modal--last-command #'execute-macro)))

(provide 'save-kmacro)
