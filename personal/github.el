;;; github.el --- Github  -*- lexical-binding: t; -*-

;;; Commentary:
;;


;;; Code:


;;; Github Trending (https://github.com/trending)
;;

(defconst github--trending-url
  "http://github-trends.ryotarai.info/rss/github_trends_all_daily.rss")

(defvar github--trending-xml nil)

(defun github--trending-request ()
  (with-temp-buffer
    (let ((download
           (expand-file-name
            (file-name-nondirectory github--trending-url) "/tmp")))
      (url-copy-file github--trending-url download)
      (insert-file-contents download)
      (prog1 (libxml-parse-xml-region (point-min) (point-max))
        (delete-file download)))))

(defun github--trending-init ()
  (unless github--trending-xml
    (setq github--trending-xml (github--trending-request)))
  (let ((repo-alist
         (cl-loop for item in (seq-filter #'listp github--trending-xml)
                  when (eq (car item) 'item)
                  for link = (cadr (assoc-default 'link item))
                  for description = (cadr (assoc-default 'description item))
                  when link
                  collect (cons description link))))
    ;; @TODO: consider language and description is nil
    (mapc
     (lambda (elt)
       (let* ((tmp (split-string (car elt) "\n"))
              (description (car tmp))
              (language (cadr tmp))
              (language (and language
                             (substring language 1 -1) ; remove '(' and ')'
                             )))
         (and description language
              (setcar
               elt
               (concat
                (if (> (string-width language) 10)
                    (propertize (helm-substring-by-width language 10)
                                'face 'font-lock-keyword-face)
                  (concat (propertize language 'face 'font-lock-keyword-face)
                          (make-string (- 13 (string-width language)) ? )))
                "   "
                description)))))
     repo-alist)))

(defvar github-trending-source
  (helm-build-sync-source (concat
                           "Github Trending"
                           " - "
                           (propertize "https://github.com/trending" 'face 'link))
    :candidates #'github--trending-init
    :action '(("Browse Link" . (lambda (candidate) (browse-url candidate))))))

;;;###autoload
(defun github-trending ()
  (interactive)
  (helm :sources '(github-trending-source)
        :buffer "*helm github trending*" :full-frame t
        :truncate-lines t))


(provide 'github)

;;; github.el ends here
