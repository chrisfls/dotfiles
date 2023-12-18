;;; -*- lexical-binding: t; -*-

(defvar ryo-modal-repeating nil
  "Whether `ryo-modal-repeat' is currently repeating.")

(defun my-ryo-modal-repeat-advice (orig-fun &rest args)
  "Advice function to set `ryo-modal-repeating' to t during execution of `ryo-modal-repeat'."
  (setq ryo-modal-repeating t)
  (unwind-protect
      (apply orig-fun args)
    (setq ryo-modal-repeating nil)))

(advice-add 'ryo-modal-repeat :around #'my-ryo-modal-repeat-advice)

(defun my/ryo-modal-repeat-two ()
  (interactive)
  (ryo-modal-repeat)
  (ryo-modal-repeat))

(defun my/ryo-modal-repeat-three ()
  (interactive)
  (my/ryo-modal-repeat-two)
  (ryo-modal-repeat))

(defun my/ryo-modal-repeat-four ()
  (interactive)
  (my/ryo-modal-repeat-three)
  (ryo-modal-repeat))

(defun my/ryo-modal-repeat-five ()
  (interactive)
  (my/ryo-modal-repeat-four)
  (ryo-modal-repeat))

(defun my/ryo-modal-repeat-six ()
  (interactive)
  (my/ryo-modal-repeat-five)
  (ryo-modal-repeat))

(defun my/ryo-modal-repeat-seven ()
  (interactive)
  (my/ryo-modal-repeat-six)
  (ryo-modal-repeat))

(defun my/ryo-modal-repeat-eight ()
  (interactive)
  (my/ryo-modal-repeat-seven)
  (ryo-modal-repeat))

(defun my/ryo-modal-repeat-nine ()
  (interactive)  
  (my/ryo-modal-repeat-eight)
  (ryo-modal-repeat))

(defun my/ryo-modal-repeat-ten ()
  (interactive)
  (my/ryo-modal-repeat-nine)
  (ryo-modal-repeat))
