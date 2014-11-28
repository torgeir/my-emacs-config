;;; init.el --- My Emacs configuration file

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
        ("melpa"       . "http://melpa.milkbox.net/packages/")
        ("marmalade"   . "http://marmalade-repo.org/packages/")))
(package-initialize)

;; Use "M-x install-important-packages" to install packages on a new machine
(defun install-important-packages ()
  "Install only the sweetest of packages."
  (interactive)
  (package-refresh-contents)
  (mapc #'(lambda (package)
            (unless (package-installed-p package)
              (package-install package)))
        '(exec-path-from-shell
	  browse-kill-ring
	  dired+
	  )))

;;;; macros
(defmacro after (mode &rest body)
  "`eval-after-load' MODE evaluate BODY."
  (declare (indent defun))
  `(eval-after-load ,mode
     '(progn ,@body)))

(after `python
  (message "python has been loaded")
  )

(after "exec-path-from-shell-autoloads"
  (exec-path-from-shell-initialize))

;;;; global key bindings

;; Mac OSX
(when (eq system-type 'darwin)
  ;; swap <command> and <alt>
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'control))

;; Quick open recent files
(require 'recentf)
(recentf-mode 1)
(global-set-key (kbd "C-x C-r") 'recentf-open-files)

;;;; emacs lisp
(defun imenu-elisp-sections ()
  (setq imenu-prev-index-position-function nil)
  (add-to-list 'imenu-generic-expression '("Sections" "^;;;; \\(.+\\)$" 1) t))

(add-hook 'emacs-lisp-mode-hook 'imenu-elisp-sections)

;;; init.el ends here
