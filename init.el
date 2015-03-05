(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(and (fboundp 'menu-bar-mode) (not (eq system-type 'darwin))
     (menu-bar-mode -1))

;; ;; Use helm from git repo
;; ;; [Facultative] Only if you have installed async.
;; (add-to-list 'load-path "~/wip/emacs-async")
;; (add-to-list 'load-path "~/wip/helm/")
;; 
;; (require 'helm-config)
;; ;; helm prefix: "C-x c" => 'C-c h
;; (global-set-key (kbd "C-c h") 'helm-command-prefix)
;; (global-unset-key (kbd "C-x c"))
;; 
;; (global-set-key (kbd "C-x C-f") 'helm-find-files)
;; (global-set-key (kbd "M-x") 'helm-M-x)
;; (global-set-key (kbd "C-x b") 'helm-mini)
;; (global-set-key (kbd "M-l") 'helm-buffers-list)
;; (global-set-key (kbd "C-x f") 'helm-recentf)
;; (global-set-key (kbd "M-y") 'helm-show-kill-ring)
;; (global-set-key (kbd "C-c h o") 'helm-occur)
;; 
;; (helm-mode 1)

;; Use org-mode from git repo
(add-to-list 'load-path "~/repos/org-mode/lisp")
(add-to-list 'load-path "~/repos/org-mode/contrib/lisp")

(add-to-list 'load-path "/usr/local/Cellar/cask/0.7.2")
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e/")

(require 'org)
(require 'ob-tangle)

(org-babel-load-file (expand-file-name "~/.emacs.d/emacs-config.org"))

(require 'weibo)
(setq weibo-consumer-key "3426280940"
      weibo-consumer-secret "9de89c9ef2caf54fc32246885a33bcb4")
