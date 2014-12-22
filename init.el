;;; init.el --- Where all the magic begins

(and (fboundp 'menu-bar-mode)
     (not (eq system-type 'darwin))
     (menu-bar-mode -1))
(mapc (lambda (mode) (when (fboundp mode) (apply mode '(-1))))
      '(tool-bar-mode
        scroll-bar-mode))

(setq inhibit-startup-message t)

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
