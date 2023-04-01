(use-package! ryo-modal)

(defun toggle-recording-macro ()
  (interactive)
  (if defining-kbd-macro
      (progn 
        (end-kbd-macro)
        (message "Paused macro recording."))
    (start-kbd-macro nil)
    (if last-kbd-macro
        (message "Resumed macro recording.")
        (message "Started macro recording."))))

(defun start-or-save-macro ()
  (interactive)
  (if defining-kbd-macro
      (progn
        (end-kbd-macro)
        (kmacro-push-ring)
        (message "Ended macro recording."))
    (setq last-kbd-macro nil)
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
