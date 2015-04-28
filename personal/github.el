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
  (cl-loop for item in (seq-filter #'listp github--trending-xml)
           when (eq (car item) 'item)
           for link = (cadr (assoc-default 'link item))
           for description = (cadr (assoc-default 'description item))
           when link
           collect (cons description link)))

(defvar github-trending-source
  (helm-build-sync-source "Github Trending"
    :candidates #'github--trending-init
    :action '(("Browse Link" . (lambda (candidate) (browse-url candidate))))))

;;;###autoload
(defun github-trending ()
  (interactive)
  (helm :sources '(github-trending-source)
        :buffer "helm github trending" :full-frame t))


(provide 'github)

;;; github.el ends here
