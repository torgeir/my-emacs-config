;;; chunyang-helm.el --- Chunayng's helm config      -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Chunyang Xu

;; Author: Chunyang Xu <xuchunyang56@gmail.com>

;;; Commentary:

;;; Code:

;;,--------------------------------------------------------------------
;;| helm-find-files and go to specified lines, eg. ~/.emacs.d/init.el:3
;;| https://github.com/emacs-helm/helm/issues/1014
;;`--------------------------------------------------------------------

(defun helm-ff-goto-line (candidate)
  (let ((linum (let ((str (with-helm-current-buffer
                            (buffer-substring-no-properties
                             (point-at-bol) (point-at-eol)))))
                 (when (string-match ":\\([0-9]+\\)\\'" str)
                   (match-string 1 str)))))
    (find-file candidate)
    (and linum (not (string= linum ""))
         (helm-goto-line (string-to-number linum) t))))

(defmethod helm-setup-user-source :after ((source helm-source-ffiles))
  (helm-source-add-action-to-source-if
   "Find file to line" 'helm-ff-goto-line source
   (lambda (_candidate)
     (let ((str (with-helm-current-buffer
                  (buffer-substring-no-properties
                   (point-at-bol) (point-at-eol)))))
       (and (not (string-match-p ffap-url-regexp str))
            (string-match-p ":\\([0-9]+\\)\\'" str))))
   0))

;;,-------------------------------------
;;| Add imenu action to buffer list
;;`-------------------------------------

(defun chunyang--helm-imenu-buffer (candidate)
  (switch-to-buffer candidate)
  (call-interactively (key-binding (kbd "C-c h i"))))

(defmethod helm-setup-user-source :after ((source helm-source-buffers))
  (helm-source-add-action-to-source-if
   "imenu" #'chunyang--helm-imenu-buffer source
   (lambda (_candidate) t)))

(provide 'chunyang-helm)
;;; chunyang-helm.el ends here
