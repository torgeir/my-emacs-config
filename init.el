;;; init.el --- My Emacs configuration file

;; Load custom.el first
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; Set badckup directory to ~/.emacs.d/backups/
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory
					       "backups"))))

;; Turn off mouse interface early in startup to avoid momentary display
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; No startup (welcome) buffer
(setq inhibit-startup-screen t)

;; Font
(when (member "DejaVu Sans Mono" (font-family-list))
  (add-to-list 'initial-frame-alist '(font . "DejaVu Sans Mono-13.5"))
  (add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono-13.5")))

;;;; package.el
(require 'package)
(setq package-archives
      '(("gnu"         . "http://elpa.gnu.org/packages/")
        ("org"         . "http://orgmode.org/elpa/")
        ("melpa"       . "http://melpa.milkbox.net/packages/")))
(package-initialize)

(defvar my-packages '(idle-highlight-mode ; Hightlight word under cursor
		      exec-path-from-shell ; Fix Mac OSX $PATH
		      helm
		      projectile
		      helm-projectile
		      ggtags
		      helm-gtags
		      company
		      yasnippet
		      ))

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
  (message "python has been loaded")
  )

;; Fix PATH on Mac OSX
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;;;; global key bindings

(global-set-key (kbd "C-x C-b") 'ibuffer)

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

(defun my-c-mode-hook ()
  (setq-default c-basic-offset 4
		c-default-style "linux")
  )
(add-hook 'c-mode-hook 'my-c-mode-hook)

;; activate whitespace-mode to view all whitespace characters
(global-set-key (kbd "C-c w") 'whitespace-mode)

;; show unncessary whitespace that can mess up your diff
(add-hook 'prog-mode-hook (lambda () (interactive) (setq show-trailing-whitespace 1)))

;; use space to indent by default
(setq-default indent-tabs-mode nil)

;; set appearance of a tab that is represented by 4 spaces
(setq-default tab-width 4)

;;;; Misc

;; "yes or no" => 'y or n"
(defalias 'yes-or-no-p 'y-or-n-p)

;; dired
(require 'dired-x)
(setq-default dired-omit-files-p t) ; Buffer-local variable
(setq dired-omit-files (concat dired-omit-files "\\|^\\..+$"))

(show-paren-mode 1)
(idle-highlight-mode 1)

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
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)

;; With clang (not work yet, using company-gtags which is enabled by default)
;; (setq company-backends (delete 'company-semantic company-backends))

(semantic-mode)
(global-semantic-idle-summary-mode 1)

;;;;;;;;;;;;;;;
;; yasnippet ;;
;;;;;;;;;;;;;;;

(require 'yasnippet)
(yas-global-mode 1)

;; Compile
(global-set-key (kbd "<f5>") (lambda ()
                               (interactive)
                               (setq-local compilation-read-command nil)
                               (call-interactively 'compile)))

;;; init.el ends here
