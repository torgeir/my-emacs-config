;;; helm-dict.e --- Dictionary with Helm interface

;;; Copyright (C) 2015 Chunyang Xu
;;
;; Author: Chunyang Xu <xuchunyang56@gmail.com>
;; URL: https://gihub.com/xuchunyang/my-emacs-config

;;; Commentary:

;;; Code:

(require 'helm)
(require 'osx-dictionary)

(defconst helm-dict--words-list-file (locate-user-emacs-file
                                      "var/google-10000-english.txt")
  "The path to Englsih word list.")

(defconst helm-dict--words-list-url
  "https://github.com/first20hours/google-10000-english/raw/master/google-10000-english.txt"
  "Most 10000 Common English Word list URL.
from URL `https://github.com/first20hours/google-10000-english/'.")

(defun helm-dict--read-word-list ()
  "Return a list of lines of a file at FILE."
  (with-temp-buffer
    (unless (file-exists-p helm-dict--words-list-file)
      (url-copy-file helm-dict--words-list-url helm-dict--words-list-file
                     nil 'keep-time))
    (insert-file-contents helm-dict--words-list-file)
    (split-string (buffer-string) "\n" t)))

;;;###autoload
(defun helm-dict ()
  "Helm interface for dictionary."
  (interactive)
  (helm :sources `((name . "English word")
                   (candidates . ,(helm-dict--read-word-list))
                   (action . (("Lookup with OS X Dictionary.app" .
                               (lambda (word)
                                 (osx-dictionary--view-result word)))
                              ("Lookup with Youdao Dictionary" .
                               (lambda (word)
                                 (youdao-dictionary--search-and-show-in-buffer
                                  word))))))))

(provide 'helm-dict)
;;; helm-dict.el ends here
