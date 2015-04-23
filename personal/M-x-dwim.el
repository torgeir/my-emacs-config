;;; M-x-dwim.el --- M-x with a context (Do What I Mean)

;;; Commentary:

;;; Code:

;;;###autoload
(defun M-x-dwim ()
  ""
  (interactive)
  ;; Man
  (require 'man)
  (let ((man-args (Man-default-man-entry)))
    ;; @TODO: Make sure man-args is available.
    (if (string= man-args "")
        (message "No item under point")
      (man man-args)))
  ;; Link supported by org-mode
  (org-open-at-point-global))

(provide 'M-x-dwim)

;;; M-x-dwim.el ends here
