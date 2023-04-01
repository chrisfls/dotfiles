
(defun go-to-end-of-region ()
  (interactive)
  (when (region-active-p)
    (when (< (point) (mark))
      (exchange-point-and-mark))
    (deactivate-mark)))

(defun go-to-beginning-of-region ()
  (interactive)
  (when (region-active-p)
    (when (> (point) (mark))
      (exchange-point-and-mark))
    (backward-char)
    (deactivate-mark)))

(provide 'region-boundaries)
