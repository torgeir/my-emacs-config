;;; chunyang-simple.e --- Simple editing functions  -*- lexical-binding: t; -*-

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

(defun chunyang-mail-can-use-osx-open ()
  "Return non-nil if the OS X \"open\" command is available for mailing."
  (and (featurep 'ns)
       (equal (executable-find "open") "/usr/bin/open")
       (memq system-type '(darwin))))

;;;###autoload
(defun chunyang-mail-insert-to-mailer ()
  "Send the message to your preferred mail client.
This requires either the OS X \"open\" command, or the freedesktop
\"xdg-email\" command to be available."
  (interactive)
  (save-excursion
    ;; FIXME? use mail-fetch-field?
    (let* ((to (progn
		 (goto-char (point-min))
		 ;; (forward-line)
		 (and (looking-at "^To: \\(.*\\)")
		      (match-string-no-properties 1))))
	   (subject (progn
		      (forward-line)
		      (and (looking-at "^Subject: \\(.*\\)")
			   (match-string-no-properties 1))))
	   (body (progn
		   (forward-line 2)
		   (if (> (point-max) (point))
		       (buffer-substring-no-properties (point) (point-max))))))
      (if (and to subject body)
	  (if (chunyang-mail-can-use-osx-open)
	      (start-process "/usr/bin/open" nil "open"
			     (concat "mailto:" to
				     "?subject=" (url-hexify-string subject)
				     "&body=" (url-hexify-string body)))
	    (start-process "xdg-email" nil "xdg-email"
			   "--subject" subject
			   "--body" body
			   (concat "mailto:" to)))
	(error "Subject, To or body not found")))))

;;;###autoload
(defun chunyang-make-another-scratch-buffer (arg)
  "Make another *scratch* buffer.
With ARG, put *scratch* buffer right."
  (interactive "P")
  (let ((buf (get-buffer-create "*scratch*")))
    (when (zerop (buffer-size buf))
      (with-current-buffer buf
        (insert initial-scratch-message)
        (lisp-interaction-mode)))
    (when arg
      (split-window-right)
      (other-window 1))
    (switch-to-buffer buf)))
;;;###autoload
(defalias 'demo #'chunyang-make-another-scratch-buffer)

;;; stole from https://github.com/expez/.emacs.d/blob/master/lisp/init-util.el
(defun chunyang-launch (command)
  "Launch an application from Emacs, with its own output
buffer. This is like asynch-shell-command but allows for any
number of processes at a time, rather than just one. If given a
prefix argument, the process's buffer is displayed."
  (interactive (list (read-shell-command (concat default-directory "$ "))))
  (let* ((name (car (split-string-and-unquote command)))
         (buffer (generate-new-buffer (concat "*" name "*"))))
    (set-process-sentinel (start-process-shell-command name buffer command)
                          'chunyang-launch-sentinel)
    (if (eq (car current-prefix-arg) 4)
        (display-buffer buffer))))

(defun chunyang-launch-sentinel (proc event)
  "Reports on changes in `chunyang-launch'ed applications."
  (message (format "%s: %s" proc event)))

;;; @TODO: finish this.
(defun chunyang-kill-all-buffer ()
  "Kill almost all buffers."
  (interactive)
  (mapc (lambda (elt)
          (with-current-buffer elt
            (save-buffer elt)))
        (seq-remove
         (lambda (elt)
           (member (buffer-name elt)
                   '("*scratch*" "*Messages*" "*Help*" "*info*")))
         (buffer-list))))

;;; Download stuffs
(defun chunyang-download-file (url file)
  "Download URL as file."
  (interactive
   (let* ((url (read-string "URL: "))
          (guess (file-name-nondirectory url))
          (file (read-file-name "Save to: " nil nil nil guess)))
     (list url file)))
  ;; Let's Check our conditions.
  (unless (executable-find "curl") (error "curl not found."))
  (if (string= url "") (error "Empty URL."))
  (if (file-exists-p file) (error "Existing file (%s)." file))
  (unless (require 'spinner nil t) (error "Package `spinner' not found."))
  ;; Looks cool. Let's do it.
  (unless (alist-get 'download spinner-types)
    (push '(download . ["下" "载" "中"]) spinner-types))
  ;; @TODO: this process reporter is not working.
  (spinner-start 'download 3)
  (unwind-protect
      (if (zerop (call-process "curl" nil nil nil
                               url
                               "-o" file))
          (message "Download (%s) done." file)
        (error "curl error."))
    (spinner-stop)))


;;; Misc
(defun chunyang-sum-numbers-in-region (beg end)
  "Sum numbers in region.  Notes the effect with `string-to-number'.

See also Emacs SE question: URL
  `http://emacs.stackexchange.com/questions/10939/sum-numbers-in-region'."
  (interactive "r")
  (message
   "%s"
   (seq-reduce
    #'+
    (mapcar #'string-to-number (split-string (buffer-substring beg end)))
    0)))

(provide 'chunyang-simple)
;;; chunyang-simple.el ends here
