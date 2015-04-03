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

(defvar helm-dict-actions
  '(("Lookup with OS X Dictionary.app" . (lambda (candidates)
                                           (osx-dictionary--view-result candidates)))
    ("Lookup with Youdao Dictionary" . (lambda (candidates)
                                         (youdao-dictionary--search-and-show-in-buffer
                                          candidates)))))

(defun helm-dict--suggest-fetch ()
  (list (concat helm-pattern "es")
        (concat helm-pattern "s")
        (concat helm-pattern "ing")))

(setq helm-dict--word-source
  `((name . "English word")
    (candidates . ;; '("skin" "sfsfa faf a")
                helm-dict--suggest-fetch
                )
    (action . (("X" . (lambda (cand)
                        (message cand)))))
    (persistent-action .
                       (lambda (cand)
                         (let ((buf (get-buffer-create "*helm dict summary*"))
                               result)
                           (setq result (format "[%s]" cand))
                           (with-current-buffer buf
                             (erase-buffer)
                             (setq cursor-type nil)
                             (insert result)
                             (goto-char (point-min)))
                           (display-buffer buf))))
    (volatile)
    (follow . 1)
    (follow-delay . 2)
    (require--pattern . 3)))

(defvar helm-dict--not-found-source
  (helm-build-dummy-source "Fallback"
    :action helm-dict-actions))

;;;###autoload
(defun helm-dict (arg)
  "Helm interface for dictionary.
If ARG is non-nil, don't any input."
  (interactive "P")
  (helm :sources '(helm-dict--word-source helm-dict--not-found-source)
        :input (unless arg (youdao-dictionary--region-or-word))
        :prompt "Lookup: "
        :buffer "*Helm Lookup words*"))

(provide 'helm-dict)
;;; helm-dict.el ends here
