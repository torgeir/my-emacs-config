;;; init.el --- My Emacs configuration file

;; Set badckup directory to ~/.emacs.d/backups/
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory
                                               "backups"))))

;; Turn off mouse interface early in startup to avoid momentary display
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; No startup (welcome) buffer
(setq inhibit-startup-screen t)

;; Setting English Font
(if (member "Monaco" (font-family-list))
    (set-face-attribute
     'default nil :font "Monaco 13"))

;;;; package.el
(require 'package)
(setq package-archives
      '(("gnu"         . "http://elpa.gnu.org/packages/")
        ("org"         . "http://orgmode.org/elpa/")
        ("melpa"       . "http://melpa.org/packages/")))
(package-initialize)

(defvar my-packages '(idle-highlight-mode ; Hightlight word under cursor
                      exec-path-from-shell ; Fix Mac OSX $PATH
                      helm
                      projectile
                      helm-projectile
                      ggtags
                      helm-gtags))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;;;; macros
(defmacro after (mode &rest body)
  "`eval-after-load' MODE evaluate BODY."
  (declare (indent defun))
  `(eval-after-load ,mode
     '(progn ,@body)))

(after `python
  (message "python has been loaded"))

;; Fix PATH on Mac OSX
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;;;; global key bindings

(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Window switch
(windmove-default-keybindings) ;; Shift+direction
(global-set-key (kbd "C-x O") (lambda () (interactive) (other-window -1))) ;; back one
(global-set-key (kbd "C-x C-o") (lambda () (interactive) (other-window 2))) ;; forward two

(defun move-line-up ()
  "Move up the current line."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

(defun move-line-down ()
  "Move down the current line."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))

(global-set-key (kbd "M-<up>") 'move-line-up)
(global-set-key (kbd "M-<down>") 'move-line-down)

;; RSS reader
(global-set-key (kbd "C-x w") 'elfeed)

;; This is set by default
;; (global-set-key (kbd "M-<f10>") 'toggle-frame-maximized)
(global-set-key (kbd "M-<f11>") 'toggle-frame-fullscreen)

;; Mac OSX
(when (eq system-type 'darwin)
  ;; swap <command> and <alt>
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'control))

;;;; emacs lisp
(defun imenu-elisp-sections ()
  (setq imenu-prev-index-position-function nil)
  (add-to-list 'imenu-generic-expression '("Sections" "^;;;; \\(.+\\)$" 1) t))

(add-hook 'emacs-lisp-mode-hook 'imenu-elisp-sections)

;;;; C
(setq-default c-default-style "linux"
              c-basic-offset 4)

;; activate whitespace-mode to view all whitespace characters
(global-set-key (kbd "C-c w") 'whitespace-mode)

;; show unncessary whitespace that can mess up your diff
(add-hook 'prog-mode-hook (lambda () (interactive) (setq show-trailing-whitespace 1)))

(defun delete-trailing-spaces-on-saving ()
  (interactive)
  (let ((y-or-n (read-from-minibuffer "Delete trailing spaces? (y or n) ")))
    (when (string= "y" y-or-n)
      (delete-trailing-whitespace))))

;; Ask if delete trailing whitespace when saving
;; (add-hook 'prog-mode-hook
;;           (lambda ()
;;             (interactive)
;;             (add-hook 'before-save-hook 'delete-trailing-spaces-on-saving)))

;; use space to indent by default
(setq-default indent-tabs-mode nil)

;; set appearance of a tab that is represented by 4 spaces
(setq-default tab-width 4)

;;;; org-mode

(require 'org)
;; highlight native code block
(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)

;; (setq org-edit-src-content-indentation 0)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (sh . t)
   (scheme . t)))

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

;; org-mode with htmlize (code syntax highlight export html)
;; http://yoo2080.wordpress.com/2013/08/26/how-to-make-rainbow-delimiters-mode-work-with-org-mode-export-or-htmlize/
(load "~/.emacs.d/elpa/htmlize/htmlize.elc")
(eval-after-load 'htmlize
  '(progn
     ;; make htmlize to handle face name strings as well
     (defadvice htmlize-attrlist-to-fstruct (around my-make-it-accept-string activate)
       (if (stringp (ad-get-arg 0))
           (progn
             (setq ad-return-value (htmlize-face-to-fstruct (intern (ad-get-arg 0)))))
         ad-do-it))))

(defvar my-htmlize-off-modes nil
  "list of minor modes to disable when using htmlize")

(defun my-htmlize-before-hook-default ()
  (dolist (mode my-htmlize-off-modes)
    (if (fboundp mode)
        (funcall mode 0)))

  (font-lock-fontify-buffer)
  (jit-lock-fontify-now)

  ;; copied from font-lock-default-function (make font-lock-face property act as alias for face property)
  (set (make-local-variable 'char-property-alias-alist)
       (copy-tree char-property-alias-alist))
  (let ((elt (assq 'face char-property-alias-alist)))
    (if elt
        (unless (memq 'font-lock-face (cdr elt))
          (setcdr elt (nconc (cdr elt) (list 'font-lock-face))))
      (push (list 'face 'font-lock-face) char-property-alias-alist))))

(add-hook 'htmlize-before-hook 'my-htmlize-before-hook-default)

;; (add-to-list 'my-htmlize-off-modes 'rainbow-delimiters-mode)

;;;; Scheme

;;;;;;;;;;;;;;;;;
;; smartparens ;;
;;;;;;;;;;;;;;;;;
;; (smartparens-global-mode t)

;;;;;;;;;;;;
;; geiser ;;
;;;;;;;;;;;;
(setq geiser-default-implementation 'racket)

;;;; Misc

;;;;;;;;;;;;;;;
;; guide key ;;
;;;;;;;;;;;;;;;
(require 'guide-key)
(setq guide-key/guide-key-sequence '("C-x" "C-x r" "C-x 4" "M-s" "C-c h"))
(guide-key-mode 1)  ; Enable guide-key-mode

;; Hightlight current line globally
;; (global-hl-line-mode)

;; Multiple cursor
(require 'multiple-cursors)
;; base on an active region
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
;; base on keywords
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; auto insert pairs
;; (electric-pair-mode 1)

;; "yes or no" => 'y or n"
(defalias 'yes-or-no-p 'y-or-n-p)

;; dired
(require 'dired-x)
(setq-default dired-omit-files-p t) ; Buffer-local variable
(setq dired-omit-files (concat dired-omit-files "\\|^\\..+$"))

(show-paren-mode 1)
;; (idle-highlight-mode 1)

;;;;;;;;;;
;; Helm ;;
;;;;;;;;;;
(require 'helm-config)

;; helm prefix: "C-x c" => 'C-c h
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

(helm-mode 1)

(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x f") 'helm-recentf)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-c h o") 'helm-occur)

;;;;;;;;;;;;;;;;
;; Projectile ;;
;;;;;;;;;;;;;;;;

(projectile-global-mode)
(setq projectile-completion-system 'helm)
(require 'helm-projectile)
(helm-projectile-on)

;;;;;;;;;;;;
;; ggtags ;;
;;;;;;;;;;;;

(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'asm-mode)
              (ggtags-mode 1))))

(require 'helm-gtags)
;; Enable helm-gtags-mode
(add-hook 'dired-mode-hook 'helm-gtags-mode)
(add-hook 'eshell-mode-hook 'helm-gtags-mode)
(add-hook 'c-mode-hook 'helm-gtags-mode)
(add-hook 'asm-mode-hook 'helm-gtags-mode)

;;;;;;;;;;;;;
;; company ;;
;;;;;;;;;;;;;
;; (require 'company)
;; (add-hook 'after-init-hook 'global-company-mode)

;; With clang (not work yet, using company-gtags which is enabled by default)
;; (setq company-backends (delete 'company-semantic company-backends))

;; (semantic-mode 1)
;; (global-semantic-idle-summary-mode 1)

;;;;;;;;;;;;;;;
;; yasnippet ;;
;;;;;;;;;;;;;;;

;; (require 'yasnippet)
;; (yas-global-mode 1)

;;;;;;;;;;;;;;
;; nyam Cat ;;
;;;;;;;;;;;;;;
;; disable it, I want to custom mode line
;; (nyan-mode 1)

;; Show column number too
(column-number-mode 1)

;; Mode line
;; see http://www.lunaryorn.com/2014/07/26/make-your-emacs-mode-line-more-useful.html
;; and http://amitp.blogspot.sg/2011/08/emacs-custom-mode-line.html

;; use smart-mode-line for now
;; (sml/setup)

;; use powerline
;; (require 'powerline)
;; (powerline-default-theme)

;; do not display some minor mode (use Diminish)
;; (eval-after-load "helm"
;;   '(diminish 'helm-mode))

;; Compile
(global-set-key (kbd "<f5>") (lambda ()
                               (interactive)
                               (setq-local compilation-read-command nil)
                               (call-interactively 'compile)))

;;;;;;;;;;;;;;
;; Flycheck ;;
;;;;;;;;;;;;;;

;; Enable globally
;; (add-hook 'after-init-hook #'global-flycheck-mode)

;; Load custom.el first
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;;;; Blog with org-mode (org-page)
;;;;;;;;;;;;;;
;; org-page ;;
;;;;;;;;;;;;;;
(require 'org-page)
(setq op/repository-directory "~/wip/blog/") ;; the repository location
(setq op/site-domain "http://xuchunyang.me") ;; your domain
;;; the configuration below you should choose one, not both
(setq op/personal-disqus-shortname "xcysblog")    ;; your disqus commenting system
(setq op/personal-google-analytics-id "UA-52627886-1")
(setq op/personal-github-link "https://github.com/xuchunyang")
(setq op/site-main-title "Chunyang Xu")
(setq op/site-sub-title "")

;;;; ERC -- Emacs irc client
(load-file "~/.emacs.d/prelude-erc.el")
(require 'prelude-erc)

(defun my-erc ()
  (interactive)
  (setq erc-nick "chunyang")
  (setq erc-password "xcy1993")
  (erc :server "irc.freenode.net" :port 6667 :nick erc-nick :password erc-password))

;; Turn off Bell
(setq ring-bell-function 'ignore)

;; Center text when only one window
(when (require 'automargin nil t)
  (automargin-mode 1))

;;; init.el ends here
