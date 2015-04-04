;;; chunyang-simple.e --- Simple editing functions

;;; Commentary:

;;; Code:

;;;###autoload
(defun chunyang-split-window-right ()
  "Split window with another buffer."
  (interactive)
  (select-window (split-window-right))
  (switch-to-buffer (other-buffer)))

(defun chunyang-format-time (time)
  "Convert 'October 20, 2014' to '2014-10'."
  (format-time-string
   "%Y-%m"
   (apply 'encode-time (parse-time-string
                        (concat time ", 12:12:12")))))

;;;###autoload
(defun chunyang-insert-current-date (arg)
  "Display current date.
With prefix argument, insert current date at point."
  (interactive "P")
  (funcall (if arg 'insert 'message) (format-time-string "%Y/%m/%d")))

(defun chunyang-region-length (start end)
  "Return the length of current region."
  (interactive (list (region-beginning) (region-end)))
  (message "[%d]" (- end start)))

(defun imenu-elisp-sections ()
  (setq imenu-prev-index-position-function nil)
  (add-to-list 'imenu-generic-expression '("Sections" "^;;; \\(.+\\)$" 1) t))

;; (add-hook 'emacs-lisp-mode-hook 'imenu-elisp-sections)

(provide 'chunyang-simple)
;;; chunyang-simple.el ends here
