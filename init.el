;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;; (package-initialize)

;;; init.el --- Emacs configuration of Chunyang Xu -*- lexical-binding: t; -*-

;;; Debugging
;; (setq debug-on-error t)
(setq ad-redefinition-action 'accept)

(add-to-list 'load-path "/Users/xcy/repos/benchmark-init-el")
(require 'benchmark-init-loaddefs)
(benchmark-init/activate)

(unless noninteractive
  (message "Loading %s..." load-file-name))


;;; Package management

;; Please don't load outdated byte code
(setq load-prefer-newer t)

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

(package-initialize)

(eval-when-compile
  (require 'use-package))

(require 'diminish)                ;; if you use :diminish
(require 'bind-key)                ;; if you use any :bind variant


;;; Requires

(require 'subr-x)
(require 'time-date)


;;; Initialization
(when (version< emacs-version "25")
  (warn "This configuration needs Emacs trunk, but this is %s!" emacs-version))

;; And disable the site default settings
(setq inhibit-default-init t)

;; Warn if the current build is more than a week old
(run-with-idle-timer
 9 nil
 (lambda ()
   (let ((time-since-build (time-subtract (current-time) emacs-build-time)))
     (when (> (time-to-number-of-days time-since-build) 7)
       (lwarn 'emacs :warning "Your Emacs build is more than a week old!")))))


;;; Environment fixup
(use-package exec-path-from-shell
  :ensure t
  :if (eq system-type 'darwin)
  :config
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "LANG")
  (exec-path-from-shell-copy-env "INFOPATH"))


;;; Customization interface
(use-package cus-edit
  :init
  (setq custom-file (locate-user-emacs-file "custom.el"))
  (load custom-file 'no-error 'no-message))


;;; OS X support
(use-package ns-win                     ; OS X window support
  :if (eq system-type 'darwin)
  :config
  (setq ns-pop-up-frames nil        ; Don't pop up new frames from the workspace
        mac-command-modifier 'meta
        mac-option-modifier 'control
        ;; ns-function-modifier 'super
        ))

(use-package lunaryorn-osx              ; Personal OS X tools
  :disabled t
  :if (eq system-type 'darwin)
  :load-path "personal/"
  :defines (lunaryorn-darwin-trash-tool)
  :config
  (if (executable-find lunaryorn-darwin-trash-tool)
      (defalias 'system-move-file-to-trash 'lunaryorn-darwin-move-file-to-trash)
    (warn "Trash support not available!
Install Trash from https://github.com/ali-rantakari/trash!
Homebrew: brew install trash")))


;;; User interface

;; Get rid of tool bar, menu bar and scroll bars.  On OS X we preserve the menu
;; bar, since the top menu bar is always visible anyway, and we'd just empty it
;; which is rather pointless.
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (and (not (eq system-type 'darwin)) (fboundp 'menu-bar-mode))
  (menu-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

;;; No startup screen and short Yes/No questions.
(setq ring-bell-function #'ignore
      inhibit-startup-screen t)
(fset 'yes-or-no-p #'y-or-n-p)
;; Opt out from the startup message in the echo area by simply disabling this
;; ridiculously bizarre thing entirely.
(fset 'display-startup-echo-area-message #'ignore)

(use-package dynamic-fonts              ; Select best available font
  ;; :disabled t
  :ensure t
  :config
  (setq dynamic-fonts-preferred-monospace-fonts
        '(
          ;; Best fonts
          "Source Code Pro"     ; https://github.com/adobe-fonts/source-code-pro
          "Anonymous Pro" ; http://www.marksimonson.com/fonts/view/anonymous-pro
          ;; Consolas and its free alternative.  Ok, but not my preference
          "Inconsolata"
          "Consolas"
          ;; Also still kind of ok
          "Fira Mono"
          ;; System fonts, as last resort
          "Menlo"
          "DejaVu Sans Mono"
          "Bitstream Vera Mono"
          "Courier New")
        dynamic-fonts-preferred-monospace-point-size (pcase system-type
                                                       (`darwin 13)
                                                       (_ 10))
        dynamic-fonts-preferred-proportional-fonts
        '(
          ;; Best, from
          ;; https://www.mozilla.org/en-US/styleguide/products/firefox-os/typeface/
          "Fira Sans"
          ;; System fonts, as last resort
          "Helvetica"
          "Segoe UI"
          "DejaVu Sans"
          "Bitstream Vera"
          "Tahoma"
          "Verdana"
          "Arial Unicode MS"
          "Arial")
        dynamic-fonts-preferred-proportional-point-size (pcase system-type
                                                          (`darwin 13)
                                                          (_ 10)))
  (dynamic-fonts-setup))

;;; TODO: Improve these ugly stuff
;; (when (member "Source Code Pro" (font-family-list))
;;   (set-face-attribute 'default nil :font "Source Code Pro 13"))

(when (member "STFangsong" (font-family-list))
  (set-fontset-font t 'han (font-spec :family "STFangsong"))
  (setq face-font-rescale-alist '(("STFangsong" . 1.3))))

(use-package zenburn-theme :ensure t)
(use-package color-theme-sanityinc-tomorrow :ensure t)
(use-package solarized-theme :ensure t)

(defcustom chunyang-theme-favourites nil
  "My favourite color themes."
  :type '(list symbol))

(require 'helm)
(defvar chunyang-theme-helm-source
  (helm-build-sync-source "My favourite color themes"
    :candidates (lambda () chunyang-theme-favourites) ; Dynamically
    :action (helm-make-actions
             "Enable theme"
             (lambda (candicate)
               (let ((current-theme (car custom-enabled-themes))
                     (new-theme     (intern candicate)))
                 (disable-theme current-theme)
                 (load-theme new-theme t))))))

(defun chunyang-switch-theme ()
  "Load one of my favourite themes."
  (interactive)
  (helm :sources '(chunyang-theme-helm-source)
        :buffer "*chunyang theme*"))

(setq chunyang-theme-favourites
      '(zenburn
        solarized-dark
        sanityinc-tomorrow-eighties
        sanityinc-tomorrow-night))

;;; The mode line

;; Standard stuff
(line-number-mode)
(column-number-mode)
(size-indication-mode)

(use-package smart-mode-line
  :disabled t
  :ensure t
  :config
  (setq sml/theme 'respectful)
  (sml/setup))

(use-package powerline
  :disabled t
  :ensure t
  :config (powerline-default-theme))

(use-package nyan-mode
  :disabled t
  :ensure t
  :config (nyan-mode 1))

(use-package which-func                 ; Current function name in header line
  :config
  (which-function-mode)
  (setq which-func-unknown "⊥"          ; The default is really boring…
        which-func-format
        `((:propertize (" ➤ " which-func-current)
                       local-map ,which-func-keymap
                       face which-func
                       mouse-face mode-line-highlight
                       help-echo "mouse-1: go to beginning\n\
  mouse-2: toggle rest visibility\n\
  mouse-3: go to end"))))


;;; The minibuffer
(use-package helm
  ;; :ensure t
  :load-path "~/wip/helm"
  :diminish helm-mode
  :config
  ;; Old value is "C-x c", needs to be changed before loading helm-config
  (setq helm-command-prefix-key "C-c h")
  (require 'helm-config)

  (helm-mode)
  (helm-adaptive-mode)
  ;; (helm-autoresize-mode)

  ;; Local map
  (bind-keys :map helm-command-map
             ("g" . helm-chrome-bookmarks)
             ("z" . helm-complex-command-history))
  ;; Global map
  (bind-keys ([remap execute-extended-command] . helm-M-x)            ; M-x
             ;; File
             ([remap find-file]                . helm-find-files)     ; C-x C-f
             ([remap set-fill-column]          . helm-recentf)        ; C-x f
             ;; Buffer
             ([remap switch-to-buffer]         . helm-mini)           ; C-x b
             ([remap downcase-word]            . helm-buffers-list)   ; M-l
             ;; Kill ring
             ([remap yank-pop]                 . helm-show-kill-ring) ; M-y
             ([remap suspend-frame]            . helm-resume)         ; C-z
             ;; Register
             ([remap jump-to-register]         . helm-register)
             ;; Help
             ([remap apropos-command]          . helm-apropos)        ; C-h a
             ;; Bookmark
             ([remap bookmark-jump]            . helm-filtered-bookmarks) ; C-x r b
             ;; Project (Git)
             ([remap list-directory]           . helm-browse-project) ; C-x C-d
             ;; TAGS
             ([remap xref-find-definitions]    . helm-etags-select)
             )

  (use-package helm-descbinds           ; Yet Another `describe-bindings' with
    :ensure t                           ; `helm', just invoke C-h b
    :config (helm-descbinds-mode))

  (require 'helm-regexp)
  ;; (defmethod helm-setup-user-source ((source helm-source-multi-occur))
  ;;   (oset source :follow 1))

  (defun my-helm-occur ()
    "Preconfigured helm for Occur."
    (interactive)
    (helm-occur-init-source)
    (let ((bufs (list (buffer-name (current-buffer)))))
      (helm-attrset 'moccur-buffers bufs helm-source-occur)
      (helm-set-local-variable 'helm-multi-occur-buffer-list bufs)
      (helm-set-local-variable
       'helm-multi-occur-buffer-tick
       (cl-loop for b in bufs
                collect (buffer-chars-modified-tick (get-buffer b)))))
    (let ((input (if (region-active-p)
                     (buffer-substring (region-beginning) (region-end))
                   (let ((symbol (symbol-at-point)))
                     (and symbol (symbol-name symbol))))))
      (helm :sources 'helm-source-occur
            :buffer "*helm occur*"
            :history 'helm-grep-history
            :input input
            :preselect (and (memq 'helm-source-occur helm-sources-using-default-as-input)
                            (format "%s:%d:" (buffer-name) (line-number-at-pos (point))))
            :truncate-lines t)))

  ;; (with-eval-after-load "helm-regexp.el"
  ;;   (setq helm-source-occur (helm-make-source "Occur" 'helm-source-multi-occur))
  ;;   (setq helm-source-occur (helm-make-source "Moccur" 'helm-source-multi-occur)))

  (bind-key "M-i" #'my-helm-occur)
  (bind-key "M-i" #'helm-occur-from-isearch isearch-mode-map)

  ;; @TODO:
  ;; (defun isearch-from-helm-occur ()
  ;;   "Invoke `isearch-forward' from helm-occur."
  ;;   (interactive)
  ;;   (setq helm-exit-status 0)
  ;;   (condition-case nil
  ;;       (exit-minibuffer)
  ;;     (call-interactively #'isearch-forward)))
  ;; (bind-key "C-s" #'isearch-from-helm-occur helm-moccur-map)

  (when (eq system-type 'darwin)
    (setq helm-grep-default-command
          "ggrep --color=never -a -d skip %e -n%cH -e %p %f"))
  (bind-key "M-I" #'helm-do-grep)

  ;; Edit grep buffer
  (use-package wgrep
    :ensure t)

  (bind-key "C-c <SPC>" #'helm-all-mark-rings)

  ;; Distinguish <TAB> and C-i, see (info "(emacs) Named ASCII Chars")
  ;; (global-set-key [tab] 'indent-for-tab-command)
  ;; (global-set-key "\C-i" 'helm-semantic-or-imenu)

  ;; http://stackoverflow.com/a/1792482/2999892
  ;; (setq local-function-key-map (delq '(kp-tab . [9]) local-function-key-map))
  ;; (global-set-key (kbd "C-i") 'helm-semantic-or-imenu)

  ;; http://stackoverflow.com/a/11319885/2999892
  ;; (define-key input-decode-map (kbd "C-i") (kbd "H-i"))
  ;; (global-set-key (kbd "H-i") 'helm-semantic-or-imenu)

  ;; ;; (global-set-key [011] 'emacs-version)

  (defun toggle-helm (arg)
    "Toggle helm.  With prefix argument, always turn off."
    (interactive "P")
    (if (or arg helm-mode)
        (progn (helm-mode -1)
               (unbind-key [remap execute-extended-command]))
      (helm-mode +1)
      (bind-key [remap execute-extended-command] #'helm-M-x)))
  )

;;; Save-minibuffer-history
;;
;;
(use-package savehist
  :config
  (setq savehist-file "~/.emacs.d/history"
        history-delete-duplicates t)
  (setq history-length 100) ; default is 30.
  (savehist-mode 1))


;;; Buffer, Windows and Frames

(setq frame-resize-pixelwise t          ; Resize by pixels
      frame-title-format
      '(:eval (if (buffer-file-name)
                  (abbreviate-file-name (buffer-file-name)) "%b")))

(use-package popwin
  :ensure t
  :config (popwin-mode 1))

(use-package frame
  :config
  ;; (global-set-key (kbd "C-z") nil)
                                        ; Disable `suspend-frame'.
  (unbind-key "C-x C-z")
  (bind-keys ("C-c T F" . toggle-frame-fullscreen)
             ("C-c T m" . toggle-frame-maximized))
  (add-to-list 'initial-frame-alist '(maximized . fullscreen)))

;;; Note: already enabled by default from Emacs 24.4 (?)
(use-package uniquify                   ; Make buffer names unique
  :config (setq uniquify-buffer-name-style 'forward))

(use-package ibuffer                    ; Better buffer list
  :bind (([remap list-buffers] . ibuffer)))

(use-package ace-window
  :ensure t
  :bind (("M-o" . ace-window)))

(use-package desktop                    ; Save buffers, windows and frames
  :config (desktop-save-mode))

(use-package winner                     ; C-c <left>, undo
  :config (winner-mode))                ; C-c <right>, redo

(use-package writeroom-mode             ; Distraction-free editing
  :ensure t
  :bind (("C-c T R" . writeroom-mode)))


;;; File handling

;; Keep backup and auto save files out of the way
(setq backup-directory-alist `((".*" . ,(locate-user-emacs-file ".backup")))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

(use-package files
  :bind (("C-c f u" . revert-buffer))
  :config
  (bind-key "C-c R" (lambda () (interactive) (revert-buffer t t)))
  ;; Use GNU ls for Emacs
  (when-let (gnu-ls (and (eq system-type 'darwin) (executable-find "gls")))
    (setq insert-directory-program gnu-ls)))

(use-package dired                      ; Edit directories
  :config
  ;; VCS integration with `diff-hl'
  ;; (add-hook 'dired-mode-hook 'diff-hl-dired-mode)
  (use-package dired-x
    :config
    (add-hook 'dired-mode-hook (lambda () (dired-omit-mode 1)))))

(use-package direx
  :ensure t
  :config
  (push '(direx:direx-mode :position left :width 25 :dedicated t)
        popwin:special-display-config)
  ;; (bind-key "C-x C-J" #'direx:jump-to-directory-other-window)
  )

(use-package neotree
  :disabled t
  :ensure t
  :bind (("C-x C-j" . neotree-toggle)))

(use-package bookmark                   ; Bookmarks for Emacs buffers
  ;; Save bookmarks immediately after a bookmark was added
  :config (setq bookmark-save-flag 1))

(use-package recentf                    ; Save recently visited files
  :config
  (setq recentf-max-saved-items 200
        recentf-max-menu-items 15
        ;; Cleanup recent files only when Emacs is idle, but not when the mode
        ;; is enabled, because that unnecessarily slows down Emacs. My Emacs
        ;; idles often enough to have the recent files list clean up regularly
        recentf-auto-cleanup 300
        recentf-exclude (list "/\\.git/.*\\'" ; Git contents
                              "/elpa/.*\\'"   ; Package files
                              "/itsalltext/"  ; It's all text temp files
                              ".*\\.gz\\'"
                              ".*-autoloads\\.el\\'"))
  (recentf-mode))

(use-package saveplace                  ; Save point position in files
  :config (setq-default save-place t))

(use-package autorevert                 ; Auto-revert buffers of changed files
  :config (global-auto-revert-mode))

(use-package launch                     ; Open files in external programs
  :ensure t
  :config (global-launch-mode))

(use-package lunaryorn-files            ; Personal file tools
  :load-path "personal/"
  :bind (("C-c f D" . lunaryorn-delete-file-and-buffer)
         ("C-c f i" . lunaryorn-find-user-init-file-other-window)
         ("C-c f o" . lunaryorn-launch-dwim)
         ("C-c f R" . lunaryorn-rename-file-and-buffer)
         ("C-c f w" . lunaryorn-copy-filename-as-kill)))

;;; Additional bindings for built-ins
(bind-key "C-c f v d" #'add-dir-local-variable)
(bind-key "C-c f v l" #'add-file-local-variable)
(bind-key "C-c f v p" #'add-file-local-variable-prop-line)


;;; Basic editing

;; Disable tabs, but given them proper width
(setq-default indent-tabs-mode nil
              tab-width 8)

(use-package electric                   ; Electric code layout
  :config (electric-layout-mode))

(use-package elec-pair                  ; Electric pairs
  :config (electric-pair-mode))

;; Indicate empty lines at the end of a buffer in the fringe, but require a
;; final new line
(setq indicate-empty-lines t
      require-final-newline t)

(setq kill-ring-max 200                 ; More killed items
      ;; Save the contents of the clipboard to kill ring before killing
      save-interprogram-paste-before-kill t)

;; Configure a reasonable fill column, indicate it in the buffer and enable
;; automatic filling
(setq-default fill-column 80)
;; (add-hook 'text-mode-hook #'auto-fill-mode)
;; (add-hook 'prog-mode-hook #'auto-fill-mode)

(use-package chunyang-simple
  :load-path "personal"
  :bind (([remap split-window-right] . chunyang-split-window-right)
         ("C-c t" . chunyang-insert-current-date)))

(use-package ws-butler
  :ensure t
  :diminish ws-butler-mode
  :config (add-hook 'prog-mode-hook 'ws-butler-mode))

(use-package adaptive-wrap              ; Choose wrap prefix automatically
  :ensure t
  :config (add-hook 'visual-line-mode-hook #'adaptive-wrap-prefix-mode))

(use-package visual-fill-column
  :ensure t
  ;; Keep the fringe
  :config
  (setq visual-fill-column-disable-fringe nil)
  (add-hook 'visual-line-mode-hook #'visual-fill-column-mode))

(use-package fill-column-indicator      ; graphically indicates the fill column
  :ensure t)

(use-package visual-regexp              ; Regexp replace with in-buffer display
  :disabled t                           ; This feature is replaced by `anzu'
  :ensure t
  :bind (("C-c r" . vr/query-replace)
         ("C-c R" . vr/replace)))

(use-package zop-to-char
  :ensure t
  :config
  (bind-keys ([remap zap-to-char] . zop-to-char)
             ("M-z"               . zop-up-to-char)))

(use-package easy-kill                  ; Easy killing and marking on C-w
  :ensure t
  :config
  (bind-keys ([remap kill-ring-save] . easy-kill)
             ([remap mark-sexp]      . easy-mark)))

(use-package align                      ; Align text in buffers
  :config
  (bind-keys ("C-c A a" . align)
             ("C-c A c" . align-current)
             ("C-c A r" . align-regexp)))

(use-package multiple-cursors           ; Edit text with multiple cursors
  :disabled t
  :ensure t
  :bind (("C-c m e"   . mc/mark-more-like-this-extended)
         ("C-c m h"   . mc/mark-all-like-this-dwim)
         ("C-c m l"   . mc/edit-lines)
         ("C-c m n"   . mc/mark-next-like-this)
         ("C-c m p"   . mc/mark-previous-like-this)
         ("C-c m r"   . vr/mc-mark)
         ("C-c m C-a" . mc/edit-beginnings-of-lines)
         ("C-c m C-e" . mc/edit-ends-of-lines)
         ("C-c m C-s" . mc/mark-all-in-region))
  :config
  (setq mc/mode-line
        ;; Simplify the MC mode line indicator
        '(:propertize (:eval (concat " " (number-to-string (mc/num-cursors))))
                      face font-lock-warning-face)))

(use-package expand-region              ; Expand region by semantic units
  :ensure t
  :bind (("C-=" . er/expand-region)))

(use-package undo-tree                  ; Branching undo
  :ensure t
  :diminish undo-tree-mode
  :config (global-undo-tree-mode))

(use-package nlinum                     ; Line numbers in display margin
  :ensure t
  :bind (("C-c T l" . nlinum-mode)))

;; Give us narrowing back!
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)

(use-package fancy-narrow
  :ensure t
  ;; :config (fancy-narrow-mode)           ; overwrite standard narrowing keys
  )


;;; Navigation and scrolling
(setq scroll-margin 0                   ; Drag the point along while scrolling
      scroll-conservatively 1000        ; Never recenter the screen while scrolling
      scroll-error-top-bottom t         ; Move to beg/end of buffer before
                                        ; signalling an error
      ;; These settings make trackpad scrolling on OS X much more predictable
      ;; and smooth
      mouse-wheel-progressive-speed nil
      mouse-wheel-scroll-amount '(1))

(use-package ace-jump-mode              ; Jump to characters in buffers
  :disabled t
  :ensure t
  :bind (("C-c SPC" . ace-jump-mode)
         ("C-c j"   . ace-jump-mode)
         ("C-c J"   . ace-jump-mode-pop-mark))
  :config
  ;; Sync marks with Emacs built-in commands
  (ace-jump-mode-enable-mark-sync))

(use-package page-break-lines           ; Turn page breaks into lines
  :ensure t
  :config (global-page-break-lines-mode)
  :diminish page-break-lines-mode)

(use-package outline                    ; Navigate outlines in buffers
  :disabled t
  :diminish outline-minor-mode
  :config (dolist (hook '(text-mode-hook prog-mode-hook))
            (add-hook hook #'outline-minor-mode)))

(use-package imenu-anywhere             ; Helm-based imenu across open buffers
  :ensure t
  :bind (("C-c i" . helm-imenu-anywhere)))

;; Code folding
(use-package origami
  :ensure t
  ;; @TODO: Setup key or add menu for this by using easy-menu, see `helm-config'
  ;; for example.
  )

(use-package yafolding
  :disabled t
  :ensure t)

;;; Search
(setq isearch-allow-scroll t)

(use-package anzu                       ; Position/matches count for isearch
  :ensure t
  :diminish anzu-mode
  :config
  (global-anzu-mode +1)
  (setq anzu-replace-to-string-separator " => ")
  (bind-key "M-%" 'anzu-query-replace)
  (bind-key "C-M-%" 'anzu-query-replace-regexp))

(use-package helm-swoop
  :disabled t
  :ensure t
  :bind (("M-i" . helm-swoop)
         ("M-I" . helm-swoop-back-to-last-point)
         ("C-c M-i" . helm-multi-swoop)
         ("C-c M-I" . helm-multi-swoop-all))
  :config
  (setq helm-swoop-speed-or-color t)
  ;; When doing isearch, hand the word over to helm-swoop
  (define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)
  ;; From helm-swoop to helm-multi-swoop-all
  (define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop)
  ;; Move up and down like isearch
  (define-key helm-swoop-map (kbd "C-r") 'helm-previous-line)
  (define-key helm-swoop-map (kbd "C-s") 'helm-next-line)
  (define-key helm-multi-swoop-map (kbd "C-r") 'helm-previous-line)
  (define-key helm-multi-swoop-map (kbd "C-s") 'helm-next-line))

(use-package pinyin-search
  :ensure t)


;;; Highlights
(use-package hl-line
  :bind (("C-c C-l" . hl-line-mode))
  :config
  (use-package hl-line+ :ensure t))

(use-package paren                      ; Highlight paired delimiters
  :config (show-paren-mode 1))

(use-package rainbow-delimiters         ; Highlight delimiters by depth
  :ensure t
  :config (dolist (hook '(text-mode-hook prog-mode-hook))
            (add-hook hook #'rainbow-delimiters-mode)))

(use-package fic-mode
  :disabled t                           ; @FIXME: not work well, try another one
  :ensure t
  :diminish fic-mode
  :config (fic-mode))

(use-package color-identifiers-mode     ; highlight each source code identifier uniquely based on its name
  :ensure t
  :config
  (global-color-identifiers-mode)
  (bind-key "C-c T c" #'global-color-identifiers-mode))


;;; Skeletons, completion and expansion

;; In `completion-at-point', do not pop up silly completion buffers for less
;; than five candidates.  Cycle instead.
(setq completion-cycle-threshold 5)

(use-package hippie-exp                 ; Powerful expansion and completion
  :bind (([remap dabbrev-expand] . hippie-expand))
  :config
  (setq hippie-expand-try-functions-list
        '(
          ;; Try to expand word "dynamically", searching the current buffer.
          try-expand-dabbrev
          ;; Try to expand word "dynamically", searching all other buffers.
          try-expand-dabbrev-all-buffers
          ;; Try to expand word "dynamically", searching the kill ring.
          try-expand-dabbrev-from-kill
          ;; Try to complete text as a file name, as many characters as unique.
          try-complete-file-name-partially
          ;; Try to complete text as a file name.
          try-complete-file-name
          ;; Try to expand word before point according to all abbrev tables.
          try-expand-all-abbrevs
          ;; Try to complete the current line to an entire line in the buffer.
          try-expand-list
          ;; Try to complete the current line to an entire line in the buffer.
          try-expand-line
          ;; Try to complete as an Emacs Lisp symbol, as many characters as
          ;; unique.
          try-complete-lisp-symbol-partially
          ;; Try to complete word as an Emacs Lisp symbol.
          try-complete-lisp-symbol)))

(use-package company                    ; Graphical (auto-)completion
  :ensure t
  :diminish company-mode
  :config
  ;; Use Company for completion
  (bind-key [remap completion-at-point] #'company-complete company-mode-map)
  (setq company-tooltip-align-annotations t
        company-minimum-prefix-length 2
        ;; Easy navigation to candidates with M-<n>
        company-show-numbers t)
  (global-company-mode))

(use-package auto-complete
  :disabled nil
  :ensure t
  :config
  (require 'auto-complete-config)
  (ac-config-default)
  (global-auto-complete-mode 1))

(use-package company-quickhelp          ; Documentation popups for Company
  :disabled t
  :ensure t
  :config (add-hook 'global-company-mode-hook #'company-quickhelp-mode))


;;; Spelling and syntax checking
(use-package flyspell
  :diminish flyspell-mode
  :config
  (setq ispell-program-name "aspell"
        ispell-extra-args '("--sug-mode=ultra"))
  (unbind-key "C-M-i" flyspell-mode-map)
  (unbind-key "C-."   flyspell-mode-map)
  (add-hook 'text-mode-hook #'flyspell-mode)
  (add-hook 'prog-mode-hook #'flyspell-prog-mode))

(use-package flycheck
  :ensure t
  :config
  (setq flycheck-emacs-lisp-load-path 'inherit)
  ;; (global-flycheck-mode 1)
  (bind-keys ("C-c T f" . global-flycheck-mode)
             ("C-c l e" . list-flycheck-errors))
  (use-package flycheck-pos-tip           ; Show Flycheck messages in popups
    :ensure t
    :config (setq flycheck-display-errors-function
                  #'flycheck-pos-tip-error-messages)))


;;; Text editing
(use-package typo
  :disabled t
  :ensure t)

(use-package iedit
  :disabled t                           ; @TODO: read manual
  :ensure t
  :config
  (bind-key [C-return] #'iedit-rectangle-mode))


;;; Other markup languages
(use-package markdown-mode
  :ensure t
  :mode (("\\`README\\.md\\'" . gfm-mode)
         ("\\.mdpp\\'"        . gfm-mode)
         ("\\.md\\'"          . markdown-mode)
         ("\\.markdown\\'"    . markdown-mode)))

(use-package yaml-mode                  ; YAML
  :ensure t
  :config
  (add-hook 'yaml-mode-hook (lambda () (run-hooks 'prog-mode-hook))))


;;; Programming utilities
(use-package compile                    ; Compile from Emacs
  :config
  (setq compilation-ask-about-save nil  ; Just save before compiling
        compilation-always-kill t       ; Just kill old compile processes before
                                        ; starting the new one
        compilation-scroll-output 'first-error ; Automatically scroll to first
                                        ; error
        ))

(use-package highlight-numbers          ; Fontify number literals
  :disabled t
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'highlight-numbers-mode))

(use-package highlight-symbol           ; Highlighting and commands for symbols
  :ensure t
  :diminish highlight-symbol-mode
  :config
  (setq highlight-symbol-on-navigation-p t)
  ;; Navigate occurrences of the symbol under point with M-n and M-p
  (add-hook 'prog-mode-hook #'highlight-symbol-nav-mode)
  ;; Highlight symbol occurrences
  (add-hook 'prog-mode-hook #'highlight-symbol-mode)
  ;; Don't tell me how many symbols at point, I'm not interested.
  (when (fboundp 'highlight-symbol-count)
    (setf (symbol-function 'highlight-symbol-count) #'ignore)))

(use-package rainbow-mode               ; Fontify color values in code
  :disabled t
  :ensure t
  :bind (("C-c T r" . rainbow-mode))
  :config (add-hook 'css-mode-hook #'rainbow-mode))


;;; Generic Lisp
(use-package paredit                    ; Balanced sexp editing
  :ensure t
  :diminish paredit-mode
  :config
  (add-hook 'emacs-lisp-mode-hook #'paredit-mode)
  ;; (unbind-key "M-r" paredit-mode-map)
  ;; (unbind-key "M-s" paredit-mode-map)

  (unbind-key "C-j" paredit-mode-map)

  ;; (bind-key "M-s M-s" 'paredit-splice-sexp paredit-mode-map)

  (use-package paredit-menu
    :ensure t))


;;; Emacs Lisp
(use-package ielm                       ; Emacs Lisp REPL
  )

(use-package eshell
  :config
  (bind-key "C-!" #'eshell-command))

(bind-key "C-c T d" #'toggle-debug-on-error)

(bind-key "M-:" #'pp-eval-expression)

(defadvice pp-display-expression (after make-read-only (expression out-buffer-name) activate)
  "Enable `view-mode' in the output buffer - if any - so it can be closed with `\"q\"."
  (when (get-buffer out-buffer-name)
    (with-current-buffer out-buffer-name
      (view-mode 1))))

(unless
    (use-package elisp-slime-nav
      :disabled t
      :ensure t
      :diminish elisp-slime-nav-mode
      :config
      (dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
        (add-hook hook 'turn-on-elisp-slime-nav-mode))
      (bind-key                         ; C-h . (`display-local-help') is for
                                        ; show tooltip in terminal, but I use
                                        ; only GUI, so make use of it.
       [remap display-local-help] #'elisp-slime-nav-describe-elisp-thing-at-point))
  ;; Invert my own wheel
  (use-package chunyang-elisp
    :load-path "personal"
    :config
    (bind-key [remap display-local-help]
              #'chunyang-elisp-describe-thing-at-point))
  (bind-key [remap describe-distribution]
            (key-binding "\C-ho")
            ;; #'describe-function-or-variable
            )
  )

(use-package ipretty
  :ensure t
  :config (ipretty-mode 1))

(use-package aggressive-indent
  :ensure t
  :diminish aggressive-indent-mode
  :config (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode))

(use-package macrostep
  :ensure t
  ;; :load-path "~/wip/macrostep"
  :bind ("C-c e" . macrostep-expand))

(use-package command-log-mode
  :ensure t)

(use-package chunyang-elisp
  :load-path "personal/"
  :commands helm-package-install
  :bind ("C-h C-." . chunyang-elisp-function-or-variable-quickhelp))


;;; Version control
(use-package diff-hl                    ; Highlight hunks in fringe
  :disabled t                           ; Replaced by `git-gutter'
  :ensure t
  :config
  ;; Highlight changes to the current file in the fringe
  (global-diff-hl-mode)
  ;; Highlight changed files in the fringe of Dired
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode)

  ;; Fall back to the display margin, if the fringe is unavailable
  (unless (display-graphic-p)
    (diff-hl-margin-mode)))

(use-package git-gutter
  :ensure t
  :diminish git-gutter-mode
  :config
  (global-git-gutter-mode t)
  (bind-keys ("C-x C-g" . git-gutter:toggle)
             ("C-x v P" . git-gutter:previous-hunk)
             ("C-x v N" . git-gutter:next-hunk)
             ("C-x v s" . git-gutter:stage-hunk)
             ("C-x v r" . git-gutter:revert-hunk)))

(use-package git-messenger
  :ensure t
  :config
  (bind-key "C-x v p" #'git-messenger:popup-message)
  (bind-key "m" #'git-messenger:copy-message git-messenger-map))

(use-package magit                      ; The one and only Git frontend
  :ensure t
  :diminish magit-auto-revert-mode
  :init (setq magit-last-seen-setup-instructions "1.4.0")
  :bind ("C-x g" . magit-status))

(use-package git-timemachine            ; Go back in Git time
  :ensure t
  :bind (("C-x v t" . git-timemachine)))


;;; Tools and utilities
(use-package projectile                 ; Project management
  :ensure t
  :config
  ;; (setq projectile-mode-line
  ;;       '(:eval (format " P[%s]" (projectile-project-name))))
  ;; Enable projectile globally
  (projectile-global-mode)
  ;; helm
  (use-package helm-projectile
    :ensure t
    :config
    (setq projectile-completion-system 'helm)
    (helm-projectile-on)
    (use-package helm-ag
      :ensure t)
    (use-package helm-ack
      :ensure t))
  ;; perspective
  (use-package perspective
    :disabled t
    :ensure t
    :config
    (persp-mode)
    (use-package persp-projectile
      :ensure t
      :config
      (bind-key "s-p" 'projectile-persp-switch-project projectile-mode-map))))

(use-package helm-open-github
  :ensure t)

(use-package helm-github-stars
  :load-path "~/wip/helm-github-stars"
  :commands (helm-github-stars helm-github-stars-fetch)
  :config
  (setq helm-github-stars-cache-file "~/.emacs.d/var/hgs-cache"
        helm-github-stars-refetch-time (/ 6.0 24))
  (bind-key "G" #'helm-github-stars helm-command-map))

(use-package jist                       ; Gist
  :ensure t
  :config (load-file "~/.private.el"))

(use-package paradox                    ; Better package menu
  ;; :ensure t
  :load-path "~/repos/paradox/"
  ;; :commands paradox-list-packages
  :config
  (setq paradox-github-token t
        paradox-execute-asynchronously nil)
  (bind-keys ("C-c l p" . paradox-list-packages)
             ("C-c l P" . package-list-packages-no-fetch)))

(use-package guide-key
  :ensure t
  ;; :load-path "~/wip/guide-key/"
  :diminish guide-key-mode
  :config
  (setq guide-key/guide-key-sequence
        '("C-h"                         ; Help
          "C-x r"                       ; Registers and Rectangle
          "C-x 4"                       ; other-window
          "C-c h"                       ; Helm
          "C-x n"                       ; Narrowing
          "C-c p"                       ; Projectile
          "C-c T"                       ; Personal Toggle commands
          "C-c l"                       ; Personal List something commands
          "C-c f"                       ; File
          "C-x v"                       ; VCS
          "C-c A"                       ; Align
          )
        guide-key/highlight-command-regexp "rectangle")
  (add-hook 'dired-mode-hook
            (lambda () (guide-key/add-local-guide-key-sequence "%")))
  (guide-key-mode 1))

(use-package keyfreq
  :ensure t
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

(use-package hydra
  :disabled t
  :ensure t)

(use-package chinese-pyim
  :disabled t
  :load-path "~/wip/chinese-pyim"
  :config
  (setq default-input-method "chinese-pyim")
  (setq pyim-dicts
        '((:name "dict1" :file "~/wip/chinese-pyim/pyim-bigdict.txt" :coding utf-8-unix))))

(use-package helm-go
  :disabled t
  :load-path "~/wip/helm-go"
  :bind (("C-c C-p" . helm-go)))

(use-package M-x-dwim
  :disabled t
  :load-path "personal"
  :bind ("<C-return>" . M-x-dwim))


;;; Net & Web & Email
(use-package circe
  :ensure t
  :config
  (load-file  "~/.private.el")
  (setq circe-network-options
        `(("Freenode"
           :nick "chunyang"
           :channels ("#emacs", "#MacPorts")
           :nickserv-password ,freenode-password))))

(use-package weibo
  :load-path "~/wip/weibo.el/"
  :config
  (load-file "~/.private.el"))

(use-package helm-zhihu-daily
  :load-path "~/wip/helm-zhihu-daily"
  :commands helm-zhihu-daily)

(use-package google-this
  :ensure t
  :diminish google-this-mode
  :config
  (google-this-mode)
  (bind-key "C-c g" #'google-this-mode-submap))

(use-package sx                         ; StackExchange client for Emacs
  :ensure t
  :commands (sx-tab-newest sx-tab-hot)
  :load-path "~/repos/sx.el")

(use-package mu4e
  :disabled t
  :load-path "/opt/local/share/emacs/site-lisp/mu4e/"
  :config
  (setq mu4e-maildir "~/Maildir"
        mu4e-drafts-folder "/[Gmail].Drafts"
        mu4e-sent-folder   "/[Gmail].Sent Mail"
        mu4e-trash-folder  "/[Gmail].Trash"
        mu4e-maildir-shortcuts '(("/INBOX"               . ?i)
                                 ("/[Gmail].Sent Mail"   . ?s)
                                 ("/[Gmail].Starred"     . ?r)
                                 ("/[Gmail].Trash"       . ?t)
                                 ("/[Gmail].All Mail"    . ?a))
        ;; allow for updating mail using 'U' in the main view:
        mu4e-get-mail-command "proxychains4 offlineimap"
        ;; update every 30 minutes
        mu4e-update-interval (* 18 60)
        ;; something about ourselves
        user-mail-address "xuchunyang56@gmail.com"
        user-full-name  "Chunyang Xu"
        mu4e-compose-signature "Chunyang Xu"
        mu4e-headers-skip-duplicates t))

(use-package notmuch
  ;; :load-path "/opt/local/share/emacs/site-lisp"
  ;; :load-path "~/.local/share/emacs/site-lisp"
  :ensure t
  :config
  (setq notmuch-fcc-dirs "[Gmail].Sent Mail") ; stores sent mail to the specified directory
  (setq message-directory "[Gmail].Drafts") ; stores postponed messages to the specified directory
  ;; (setq notmuch-saved-searches
  ;;       '((:name "inbox" :query "tag:inbox")
  ;;         (:name "unread" :query "tag:inbox AND tag:unread")))
  user-mail-address "xuchunyang56@gmail.com"
  user-full-name  "Chunyang Xu")


;;; Dictionary
(use-package youdao-dictionary
  :ensure t
  ;; :load-path "~/wip/youdao-dictionary.el"
  :bind (("C-c y" . youdao-dictionary-search-at-point)
         ("C-c Y" . youdao-dictionary-search-at-point+))
  :config
  (setq url-automatic-caching t)
  (push "*Youdao Dictionary*" popwin:special-display-config))

(use-package osx-dictionary
  :ensure t
  :bind (("C-c d" . osx-dictionary-search-pointer))
  :config
  (push "*osx-dictionary*" popwin:special-display-config))

(use-package helm-dict
  :disabled t
  :load-path "personal/"
  :bind ("M-4" . helm-dict))

(use-package langtool
  :ensure t
  :disabled t
  :config
  (setq langtool-language-tool-jar "~/Downloads/LanguageTool-2.8/languagetool-commandline.jar"
        langtool-mother-tongue "en"))


;;; Org-mode
(use-package org
  :bind (("C-c a"   . org-agenda)
         ("C-c c"   . org-capture)
         ("C-c L"   . org-store-link)
         ("C-c C-o" . org-open-at-point-global))
  :config
  (setq org-default-notes-file "~/org/task.org"
        org-agenda-files '("~/org/task.org")
        org-capture-templates
        `(("t" "Todo" entry (file+headline "~/org/task.org" "Tasks")
           "* TODO %?\n  %i\n%a")
          ("i" "Inbox" entry (file+headline "~/org/task.org" "Inbox")
           "* %?\n  %i\n%a")))
  (org-babel-do-load-languages 'org-babel-load-languages
                               '((emacs-lisp . t)
                                 (sh . t)
                                 (scheme . t))))


;;; Online help
(use-package info                       ; Info manual viewer
  :config
  ;; Fix the stupid `Info-quoted' face.  Courier is an abysmal face, so go back
  ;; to the default face.
  (set-face-attribute 'Info-quoted nil :family 'unspecified
                      :inherit font-lock-type-face))


;;; MacPorts related tools
(use-package tcl-mode
  :mode "Portfile")

(bind-key "C-h C-k" #'find-function-on-key)
;; (bind-key "C-h h" #'describe-personal-keybindings)


;;; Web Development
(use-package restclient
  :ensure t)


;; https://github.com/Malabarba/elisp-bug-hunter
(use-package bug-hunter
  :ensure t)
