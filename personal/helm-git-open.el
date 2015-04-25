;;; helm-git-open.el --- git open  -*- lexical-binding: t; -*-

;;; Commentary:

;; or just run "M-! git open" for now, no needs for this.

;;; Code:

(defun git-open ()
  "Front end for 'git-open' shell command."
  (interactive)
  (with-temp-buffer
    (let ((rtv (condition-case nil
                   (call-process "git" nil '(t nil) nil "open")
                 (error nil))))
      (or (zerop rtv)
          (user-error "%s"
                      (replace-regexp-in-string "\n" "" (buffer-string)))))))

(provide 'helm-git-open)
;;; helm-git-open.el ends here
