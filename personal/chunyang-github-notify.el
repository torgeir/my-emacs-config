;;; chunyang-github-notify.el --- Show Github notification numbers in modelien

;;; Copyright (C) 2015 Chunyang Xu
;;
;; Author: Chunyang Xu <xuchunyang56@gmail.com>
;; URL: https://gihub.com/xuchunyang/my-emacs-config

;;; Commentary:

;; TODO:
;; - [ ] Fetch my Github notification
;; - [ ] Shwo result (if not zero) in modeline
;;       (using Unicode number, or even Github icon?
;; - [ ] Set up at timer

;;; Code:

(defvar chunyang-github-refetch-interval 15
  "Refetch period (min.")

(provide 'chunyang-github-notify)

;;; chunyang-github-notify.el ends here
