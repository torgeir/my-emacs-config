(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(and (fboundp 'menu-bar-mode) (not (eq system-type 'darwin))
     (menu-bar-mode -1))

(require 'org)
(require 'ob-tangle)
(org-babel-load-file (expand-file-name "~/.emacs.d/emacs-config.org"))
