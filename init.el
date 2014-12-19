;;; init.el --- Where all the magic begins
;;
;; This file loads Org-mode and then loads the rest of our Emacs initialization from Emacs lisp
;; embedded in literate Org-mode files.

(and (fboundp 'menu-bar-mode)
     (not (eq system-type 'darwin))
     (menu-bar-mode -1))
(mapc (lambda (mode) (when (fboundp mode) (apply mode '(-1))))
      '(tool-bar-mode
        scroll-bar-mode))

;;;; package.el
(require 'package)
(setq package-archives
      '(("gnu"         . "http://elpa.gnu.org/packages/")
        ("org"         . "http://orgmode.org/elpa/")
        ("melpa"       . "http://melpa.org/packages/")))
(package-initialize)

;;; load up main init org file
(org-babel-load-file (expand-file-name "emacs-init.org" user-emacs-directory))

;;; init.el ends here
