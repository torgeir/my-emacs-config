;;; chunyang-elisp.el ---

;;; Commentary:

;;; Code:

(defun string-first-line (string)
  (and (stringp string)
       (string-match ".*$" string)
       (match-string 0 string)))

;;;###autoload
(defun chunyang-elisp-function-or-variable-quickhelp (symbol)
  "Display a short documentation of the function or variable using `popup'.

See also `describe-function-or-variable'."
  (interactive
   (let* ((v-or-f (variable-at-point))
          (found (symbolp v-or-f))
          (v-or-f (if found v-or-f (function-called-at-point)))
          (found (or found v-or-f)))
     (list v-or-f)))
  (if (not (and symbol (symbolp symbol)))
      (message "You didn't specify a function or variable.")
    (let* ((fdoc (when (fboundp symbol)
                   (or (documentation symbol t) "Not documented.")))
           (fdoc-short (string-first-line fdoc))
           (vdoc (when  (boundp symbol)
                   (or (documentation-property symbol 'variable-documentation t)
                       "Not documented as a variable.")))
           (vdoc-short (string-first-line vdoc)))
      (and (require 'popup nil t)
           (popup-tip
            (or
             (and fdoc-short vdoc-short
                  (concat fdoc-short "\n\n"
                          (make-string 30 ?-) "\n" (symbol-name symbol)
                          " is also a " "variable." "\n\n"
                          vdoc-short))
             fdoc-short
             vdoc-short)
            :margin t
            ;; @TODO: customize `popup-tip-face', read '(info (elisp) Faces)'
            )))))

;;;###autoload
(defun sanityinc/cl-libify-next ()
  "Find next symbol from 'cl and replace it with the 'cl-lib equivalent."
  (interactive)
  (let ((case-fold-search nil))
    (re-search-forward
     (concat
      "("
      (regexp-opt
       ;; Not an exhaustive list
       '("loop" "incf" "plusp" "first" "decf" "minusp" "assert"
         "case" "destructuring-bind" "second" "third" "defun*"
         "defmacro*" "return-from" "labels" "cadar" "fourth"
         "cadadr") t)
      "\\_>")))
  (let ((form (match-string 1)))
    (backward-sexp)
    (cond
     ((string-match "^\\(defun\\|defmacro\\)\\*$")
      (kill-sexp)
      (insert (concat "cl-" (match-string 1))))
     (t
      (insert "cl-")))
    (when (fboundp 'aggressive-indent-indent-defun)
      (aggressive-indent-indent-defun))))

(require 'helm)
(require 'package)

(defun helm-package-open-homepage (candidate)
  "Helm Action."
  (let* ((pkg (intern candidate))
         (desc
          (or
           (if (package-desc-p pkg) pkg)
           (cadr (assq pkg package-alist))
           (let ((built-in (assq pkg package--builtins)))
             (if built-in
                 (package--from-builtin built-in)
               (cadr (assq pkg package-archive-contents))))))
         (extras (and desc (package-desc-extras desc)))
         (homepage (cdr (assoc :url extras))))
    (if homepage
        (browse-url homepage)
      (message "No homepage for '%S" pkg))))

(defvar helm-package-install-source
  (helm-build-sync-source "Install Package"
    :candidates (delq nil
                      (mapcar
                       (lambda (elt)
                         (unless (package-installed-p (car elt))
                           (symbol-name (car elt))))
                       package-archive-contents))
    :action (helm-make-actions
             "Install"
             (lambda (candidate) (package-install (intern candidate)))
             "Describe"
             (lambda (candidate)
               (describe-package (intern candidate)))
             "Open homepage"
             #'helm-package-open-homepage)))

(defvar helm-package-installed-source
  (helm-build-sync-source "Reinstall package"
    :candidates (delq nil
                      (mapcar (lambda (elt)
                                (when (package-installed-p (car elt))
                                  (symbol-name (car elt))))
                              package-archive-contents))
    :action (helm-make-actions
             "Reinstall"
             (lambda (candidate) (package-reinstall (intern candidate)))
             ;; "Uninstall"
             ;; (lambda (candidate)
             ;;   (and (y-or-n-p
             ;;         (format "Are you sure to uninstall %s ?" candidate))
             ;;        (package-delete
             ;;         (cadr (assq (intern candidate) package-alist)))))
             "Describe"
             (lambda (candidate) (describe-package (intern candidate)))
             "Open homepage"
             #'helm-package-open-homepage)))

;;;###autoload
(defun helm-package-install ()
  "Preconfigured `package-install' by indicating installed packages."
  (interactive)
  (helm :sources '(helm-package-install-source helm-package-installed-source)
        :buffer "*Helm package install*"
        :candidate-number-limit 9999))

(provide 'chunyang-elisp)

;;; chunyang-elisp.el ends here
