;;; helm-dict.e --- Dictionary with Helm interface

;;; Copyright (C) 2015 Chunyang Xu
;;
;; Author: Chunyang Xu <xuchunyang56@gmail.com>
;; URL: https://gihub.com/xuchunyang/my-emacs-config

;;; Commentary:

;;; Code:

(require 'helm)
(require 'osx-dictionary)

(defvar helm-dict--words-list-file
  "~/wip/google-10000-english/google-10000-english.txt")

(defun helm-dict--read-lines (file)
  "Return a list of lines of a file at FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (split-string (buffer-string) "\n" t)))

;;;###autoload
(defun helm-dict ()
  "Helm interface for dictionary."
  (interactive)
  (helm :sources `((name . "English word")
                   (candidates . ,(helm-dict--read-lines
                                   helm-dict--words-list-file))
                   (action . (("Lookup with OS X Dictionary.app" .
                               (lambda (word)
                                 (osx-dictionary--view-result word)))
                              ("Lookup with Youdao Dictionary" .
                               (lambda (word)
                                 (youdao-dictionary--search-and-show-in-buffer
                                  word))))))))

(provide 'helm-dict)
;;; helm-dict.el ends here
