;;; helm-dict.e --- Dictionary with Helm interface

;;; Copyright (C) 2015 Chunyang Xu
;;
;; Author: Chunyang Xu <xuchunyang56@gmail.com>
;; URL: https://gihub.com/xuchunyang/my-emacs-config

;;; Commentary:

;; TODO:
;; - [ ] Support Chinese to English (try to use input method?
;; - [ ] Save word (use one helm action)
;; - [ ] Show word details (fetch from Youdao?)
;; - [ ] Support more dictionary for a better default result (e.g., command line
;;       tool fanyi - 金山词霸的例句)
;; - [ ] (?) Use Server to let apps outside Emacs to use this dictionary too

;;; Code:

(require 'helm)
(require 'osx-dictionary)
(require 'youdao-dictionary)

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
(defun helm-dict (&optional arg)
  "Helm interface for dictionary.
If ARG is non-nil, don't any input."
  (interactive "P")
  (helm :sources `((name . "English word")
                   (candidates . ,(helm-dict--read-word-list))
                   (action . (("Lookup with OS X Dictionary.app" .
                               (lambda (word)
                                 (osx-dictionary--view-result word)))
                              ("Lookup with Youdao Dictionary" .
                               (lambda (word)
                                 (youdao-dictionary--search-and-show-in-buffer
                                  word))))))
        :input (when (null arg)
                 (youdao-dictionary--region-or-word))))

(provide 'helm-dict)
;;; helm-dict.el ends here
