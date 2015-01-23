(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(and (fboundp 'menu-bar-mode) (not (eq system-type 'darwin))
     (menu-bar-mode -1))

;; Use org-mode from git repo
(add-to-list 'load-path "~/repos/org-mode/lisp")
(add-to-list 'load-path "~/repos/org-mode/contrib/lisp")

(add-to-list 'load-path "/usr/local/Cellar/cask/0.7.2")
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e/")

(require 'org)
(require 'ob-tangle)

(org-babel-load-file (expand-file-name "~/.emacs.d/emacs-config.org"))
