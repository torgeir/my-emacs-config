;;; chunyang-simple.e --- Simple editing functions

;;; Commentary:

;;; Code:

;;;###autoload
(defun chunyang-split-window-right ()
  "Split window with another buffer."
  (interactive)
  (select-window (split-window-right))
  (switch-to-buffer (other-buffer)))

(provide 'chunyang-simple)
;;; chunyang-simple.el ends here
