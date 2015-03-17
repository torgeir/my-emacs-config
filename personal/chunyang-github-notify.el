;;; chunyang-github-notify.el --- Show Github notification numbers in modelien

;;; Copyright (C) 2015 Chunyang Xu
;;
;; Author: Chunyang Xu <xuchunyang56@gmail.com>
;; URL: https://gihub.com/xuchunyang/my-emacs-config

;;; Commentary:

;; TODO:
;; - [x] Fetch my Github notification
;; - [ ] Shwo result (if not zero) in modeline
;;       (using Unicode number, or even Github icon?
;; - [ ] Set up at timer (must to be a minor mode? there are must some other way
;;       to do this)
;; - [ ] Parse JSON with pure ELisp

;;; Code:

(defvar chunyang-github-notify-token nil
  "Notification scope must be enabled.")

(defconst chunyang-github-notify-api
  "https://api.github.com/notifications")

(defun chunyang-github-notify--get-conut ()
  "Return nil if failed."
  (let ((rtv (shell-command-to-string   ; FIXME: better need error check
              (format
               "curl --silent -H 'Authorization: token %s' %s | jq '. | length'"
               chunyang-github-notify-token
               chunyang-github-notify-api))))
    (when (<= (length rtv) 3)            ; indicate 99 as the max conut
      (string-to-number rtv))))

(defun chunyang-github-notify ()
  "Fetch notifications conut and update modeline."
  (let ((conut (chunyang-github-notify--get-conut)))
    (if (not (null conut))
        (setq mode-line-format (format "Github [%d]" conut))
      (user-error "[Github] Get notifications failed"))))

(provide 'chunyang-github-notify)

;;; chunyang-github-notify.el ends here
