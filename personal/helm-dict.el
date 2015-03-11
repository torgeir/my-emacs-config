;;; helm-dict.e --- Dictionary with Helm interface

;;; Copyright (C) 2015 Chunyang Xu
;;
;; Author: Chunyang Xu <xuchunyang56@gmail.com>
;; URL: https://gihub.com/xuchunyang/my-emacs-config

;;; Commentary:

;; TODO:
;; - [ ] Support Chinese to English (try to use input method?
;; - [ ] Save word (use one helm action), 单词本、搜索记录、词汇本等等
;; - [ ] Show word details (fetch from Youdao?)
;; - [ ] Support more dictionary for a better default result (e.g., command line
;;       tool fanyi - 金山词霸的例句)
;; - [ ] (?) Use Server to let apps outside Emacs to use this dictionary too
;; - [x] ignore case (Server v.s. server) when filtering English word
;; - [x] Get input anyway even if there is no candidates (also try to expand
;;       candidates)
;; - [ ] highlighting matched pattern in helm buffer

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


;;; Helm related
(defvar helm-dict--word-list
  (helm-dict--read-word-list))

(defvar helm-dict--word-source
  `((name . "English word")
    (candidates . ,helm-dict--word-list)
    (action . (("Lookup with OS X Dictionary.app" .
                (lambda (word) (osx-dictionary--view-result word)))
               ("Lookup with Youdao Dictionary" .
                (lambda (word) (youdao-dictionary--search-and-show-in-buffer
                                word)))))))

(defvar helm-dict--not-found-source
  '((name . "fallback")
    (dummy)
    (action . (("Lookup with OS X Dictionary.app" .
                (lambda (word) (osx-dictionary--view-result word)))
               ("Lookup with Youdao Dictionary" .
                (lambda (word) (youdao-dictionary--search-and-show-in-buffer
                                word)))))))

(defvar helm-dict--lookup-history nil)

;;;###autoload
(defun helm-dict (arg)
  "Helm interface for dictionary.
If ARG is non-nil, don't any input."
  (interactive "P")
  (helm :sources '(helm-dict--word-source helm-dict--not-found-source)
        :input (unless arg (youdao-dictionary--region-or-word))
        :history 'helm-dict--lookup-history
        :case-fold-search t
        :prompt "Lookup: "
        :buffer "*Helm Lookup words*"))

(provide 'helm-dict)
;;; helm-dict.el ends here
