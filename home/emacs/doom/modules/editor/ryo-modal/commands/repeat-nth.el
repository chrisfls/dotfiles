(use-package! ryo-modal)

(defvar ryo-modal-repeating nil
  "Whether `ryo-modal-repeat' is currently repeating.")

(defun my-ryo-modal-repeat-advice (orig-fun &rest args)
  "Advice function to set `ryo-modal-repeating' to t during execution of `ryo-modal-repeat'."
  (setq ryo-modal-repeating t)
  (unwind-protect
      (apply orig-fun args)
    (setq ryo-modal-repeating nil)))

(advice-add 'ryo-modal-repeat :around #'my-ryo-modal-repeat-advice)

(defun repeat-two ()
  (interactive)
  (ryo-modal-repeat)
  (ryo-modal-repeat))

(defun repeat-three ()
  (interactive)
  (repeat-two)
  (ryo-modal-repeat))

(defun repeat-four ()
  (interactive)
  (repeat-three)
  (ryo-modal-repeat))

(defun repeat-five ()
  (interactive)
  (repeat-four)
  (ryo-modal-repeat))

(defun repeat-six ()
  (interactive)
  (repeat-five)
  (ryo-modal-repeat))

(defun repeat-seven ()
  (interactive)
  (repeat-six)
  (ryo-modal-repeat))

(defun repeat-eight ()
  (interactive)
  (repeat-seven)
  (ryo-modal-repeat))

(defun repeat-nine ()
  (interactive)  
  (repeat-eight)
  (ryo-modal-repeat))

(defun repeat-ten ()
  (interactive)
  (repeat-nine)
  (ryo-modal-repeat))

(provide 'repeat-nth)
