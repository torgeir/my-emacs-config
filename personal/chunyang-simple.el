;;; chunyang-simple.e --- Simple editing functions

;;; Commentary:

;;; Code:

;;;###autoload
(defun chunyang-split-window-right ()
  "Split window with another buffer."
  (interactive)
  (split-window-right)
  (other-window 1)
  (switch-to-buffer (other-buffer)))

(provide 'chunyang-simple)
;;; chunyang-simple.el ends here
