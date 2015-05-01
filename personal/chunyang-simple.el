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

(defun chunyang--notmuch-unread-count ()
  "Number of unread mails by notmuch."
  (string-to-number
   (shell-command-to-string
    "notmuch search tag:unread | wc -l")))


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
(defun chunyang-download-file-old (url file)
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

(defun chunyang-download-file (url file)
  "Same as `chunyang-download-file-old' but use `url-copy-file' instead of curl(1)."
  (interactive
   (let* ((url (read-string "URL: "))
          (guess (file-name-nondirectory url))
          (file (read-file-name "Save to: " nil nil nil guess)))
     (list url file)))
  (unless (alist-get 'download spinner-types)
    (push '(download . ["下" "载" "中"]) spinner-types))
  ;; @TODO: this process reporter is not working.
  (spinner-start 'download 3)
  (url-copy-file url file)
  (spinner-stop))

;;; @TODO: git clone repository
(defun chunyang-git-clone (url dir)
  ""
  (interactive))


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


;;; Debug and Log
(defvar chunyang-debug-buffer "*Debug Chunyang Log*")

(defvar chunyang-debug nil
  "If non-nil, write log message into `chunyang-debug-buffer' buffer.
It is disabled by default because `chunyang-debug-buffer' grows quickly.")

;; Utility: logging
(defun chunyang-log (format-string &rest args)
  "Log message `chunyang-debug' is non-nil.
Messages are written to the `chunyang-debug-buffer' buffer.

Argument FORMAT-STRING is a string to use with `format'.
Use optional arguments ARGS like in `format'."
  (when chunyang-debug
    (with-current-buffer (get-buffer-create chunyang-debug-buffer)
      (outline-mode)
      (buffer-disable-undo)
      (set (make-local-variable 'inhibit-read-only) t)
      (goto-char (point-max))
      (insert (let ((tm (current-time)))
                (format (concat (if (string-match "Start session" format-string)
                                    "* " "** ")
                                "%s.%06d (%s)\n %s\n")
                        (format-time-string "%H:%M:%S" tm)
                        (nth 2 tm)
                        (chunyang-log-get-current-function)
                        (apply #'format (cons format-string args))))))))

(defun chunyang-log-get-current-function ()
  "Get function name calling `chunyang-log'.
The original idea is from `tramp-debug-message'."
  (cl-loop with exclude-func-re = "^chunyang-\\(?:interpret\\|log\\|.*funcall\\)"
           for btn from 1 to 40
           for btf = (cl-second (backtrace-frame btn))
           for fn  = (if (symbolp btf) (symbol-name btf) "")
           if (and (string-match "^chunyang" fn)
                   (not (string-match exclude-func-re fn)))
           return fn))

;;; Key binding
;;
;;

;;; `forward-line'
(bind-key "C-S-n" #'chunyang-line-adjust)
(defun chunyang-line-adjust (inc)
  "Move cursor vertically down/up INC lines."
  (interactive "p")
  (let ((ev last-command-event)
        (echo-keystrokes nil))
    (let* ((base (event-basic-type ev))
           (step
            (pcase base
              (?n inc)
              (?p (- inc))
              (t inc))))
      (forward-line step)
      (message "Use n,p for further move lines.")
      (set-transient-map
       (let ((map (make-sparse-keymap)))
         (dolist (mods '(() (control)))
           (dolist (key '(?n ?p))
             (define-key map (vector (append mods (list key)))
               (lambda ()               ; `lexical-binding' must be t
                 (interactive)
                 (chunyang-line-adjust (abs inc)))
               )))
         map)))))

;;; C-x [, C-x ]
(bind-key [remap backward-page] #'chunyang-page-adjust)
(bind-key [remap forward-page]  #'chunyang-page-adjust)
(defun chunyang-page-adjust (inc)
  ""
  (interactive "p")
  (let ((ev last-command-event)
        (echo-keystrokes nil))
    (let* ((base (event-basic-type ev))
           (step
            (pcase base
              (?\[ (- inc))
              (?\] inc)
              (t  inc))))
      (forward-page step)
      (message "Use [,] for further move.")
      (set-transient-map
       (let ((map (make-sparse-keymap)))
         (dolist (mods '(() (control)))
           (dolist (key '(91 93))
             (define-key map (vector (append mods (list key)))
               (lambda () (interactive) (chunyang-page-adjust (abs inc))))))
         map)))))

;; `other-window'
;;
;; from 'C-x o C-x z z z z' to 'C-x o o o o'
;;

(define-key global-map [remap other-window] #'chunyang-other-window)
(defun chunyang-other-window (count)
  (interactive "p")
  (if (eq (selected-window) (next-window))
      ;; (other-window count)
      nil                               ; no other window, do nothing
    (let ((ev last-command-event)
          (echo-keystrokes nil))
      (let* ((base (event-basic-type ev))
             (step
              (pcase base
                (?o count)
                (t  count))))
        (other-window step)               ; @TODO: when there is only one window, it should not work like this.
        (message "use o for future.")
        (set-transient-map
         (let ((map (make-sparse-keymap)))
           (dolist (mods '(() (control)))
             (dolist (key '(o))
               (define-key map (vector (append mods (list key)))
                 (lambda () (interactive) (chunyang-other-window (abs count))))))
           map))))))

;;; M-x other-window RET RET RET
;;; C-x o o o o
(defun foo--other-window (orig-fun &rest args)
  "DOC"
  (apply orig-fun args)
  (set-transient-map
   (let ((map (make-sparse-keymap)))
     (define-key map (vector last-command-event) #'repeat)
     map)))


;;; key Binding --- Repeat for a while

(defvar repeat-commands-list
  '(other-window
    next-buffer
    backward-page
    forward-page))

(defun repeat-and-repeat (orig-fun &rest args)
  (apply orig-fun args)

  (when (called-interactively-p 'interactive)
    (set-transient-map
     (let ((map (make-sparse-keymap)))
       (define-key map (vector last-command-event) #'repeat)
       map))))

(defun repeat-add ()
  (interactive)
  (dolist (command repeat-commands-list)
    (advice-add command :around #'repeat-and-repeat)))

(defun repeat-clear ()
  (interactive)
  (dolist (command repeat-commands-list)
    (advice-remove command #'repeat-and-repeat)))

(repeat-add)


;;; @TODO:
;; (helm-define-key-with-subkeys global-map
;;   (kbd "C-x v n") ?n #'git-gutter:next-hunk
;;   '((?p . git-gutter:previous-hunk)))


;;; @TODO: add timeout support

;;; Use functions from `helm' package
;;
;; * `helm-define-key-with-subkeys'

;;; `other-window'
;; (helm-define-key-with-subkeys global-map [?\C-x ?o] ?o #'other-window)

;;; `forward-page' / `backward-page'
;; (helm-define-key-with-subkeys global-map
;;   (kbd "C-x ]") ?\] #'forward-page
;;   '((?\[ . backward-page)))

;; (helm-define-key-with-subkeys global-map
;;   (kbd "C-x [") ?\[ #'backward-page
;;   '((?\] . forward-page)))

(provide 'chunyang-simple)
;;; chunyang-simple.el ends here
