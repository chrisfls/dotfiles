(use-package! ryo-modal)

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
