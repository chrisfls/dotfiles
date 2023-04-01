;; TODO: if possible stop terminating macros on any warning

(defvar saved-kmacro nil)

(defun start-or-save-kmacro ()
  (interactive)
  (if defining-kbd-macro
      (progn
        (end-kbd-macro)
        (setq saved-kmacro last-kbd-macro))
    (start-kbd-macro nil)))

(defun execute-saved-kmacro ()
  (interactive)
  (if (null saved-kmacro)
      (message "No saved macro.")
    (ignore-errors (execute-kbd-macro saved-kmacro))
    (message "Executed saved macro.")))

(defun start-or-cancel-kmacro ()
  (interactive)
  (if defining-kbd-macro
      (setq defining-kbd-macro nil)
    (start-kbd-macro nil)))

    ;; (let ((debug-on-error nil))
    ;;   (start-kbd-macro nil))))

(provide 'save-kmacro)
