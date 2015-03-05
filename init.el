;;; init.el --- Emacs configuration of Chunyang Xu -*- lexical-binding: t; -*-

;;; Copyright (C) 2015 Chunyang Xu <xuchunyang56@gmail.com>
;;
;; Author: Chunyang Xu <xuchunyang56@gmail.com>
;; URL: https://gihub.com/xuchunyang/
;; keywords: conventions

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 3 of the License, or (at your option) any later
;; version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.

;; You should have received a copy of the GNU General Public License along with
;; GNU Emacs; see the file COPYING.  If not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
;; USA.

;;; Commentary:

;; Emacs configuration inspired by https://gihub.com/lunaryorn/.emacs.d

;;; Code:

;;; Debugging
(setq message-log-max 10000)


;;; Package management

;; Please don't load outdated byte code
(setq load-prefer-newer t)

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)


;;; Requires

(require 'subr-x)
(require 'rx)
(require 'time-date)


;;; Initialization
(when (version< emacs-version "25")
  (warn "This configuration needs Emacs trunk, but this is %s!" emacs-version))

;; And disable the site default settings
(setq inhibit-default-init t)

;; Warn if the current build is more than a week old
(run-with-idle-timer
 2 nil
 (lambda ()
   (let ((time-since-build (time-subtract (current-time) emacs-build-time)))
     (when (> (time-to-number-of-days time-since-build) 7)
       (lwarn 'emacs :warning "Your Emacs build is more than a week old!")))))


;;; Environment fixup
(use-package exec-path-from-shell
  :ensure t
  :if (and (eq system-type 'darwin) (display-graphic-p))
  :init
  (progn
    (when (string-match-p "/zsh$" (getenv "SHELL"))
      ;; Use a non-interactive shell.  We use a login shell, even though we have
      ;; our paths setup in .zshenv.  However, OS X adds global settings to the
      ;; login profile.  Notably, this affects /usr/texbin from MacTeX
      (setq exec-path-from-shell-arguments '("-l")))

    (dolist (var '("EMAIL" "PYTHONPATH" "INFOPATH"))
      (add-to-list 'exec-path-from-shell-variables var))

    (exec-path-from-shell-initialize)

    (setq user-mail-address (getenv "EMAIL"))

    ;; Re-initialize the `Info-directory-list' from $INFOPATH.  Since package.el
    ;; already initializes info, we need to explicitly add the $INFOPATH
    ;; directories to `Info-directory-list'.
    (with-eval-after-load 'info
      (dolist (dir (parse-colon-path (getenv "INFOPATH")))
        (when dir
          (add-to-list 'Info-directory-list dir))))))


;;; Customization interface
(defconst chunyang-custom-file (locate-user-emacs-file "custom.el")
  "File used to store settings from Customization UI.")

(use-package cus-edit
  :defer t
  :config
  (setq custom-file chunyang-custom-file
        custom-buffer-done-kill nil               ; Kill when existing
        custom-buffer-verbose-help nil  ; Remove redundant help text
        ;; Show me the real variable name
        custom-unlispify-tag-names nil
        custom-unlispify-menu-entries nil)
  :init (load chunyang-custom-file 'no-error 'no-message))


;;; OS X support
(use-package ns-win                     ; OS X window support
  :defer t
  :if (eq system-type 'darwin)
  :config
  (setq ns-pop-up-frames nil            ; Don't pop up new frames from the workspace
        mac-command-modifier 'meta
        mac-option-modifier 'control))


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
  :ensure t
  :init
  (progn
    (setq dynamic-fonts-preferred-monospace-fonts
          '(
            ;; Best fonts
            "Source Code Pro"   ; https://github.com/adobe-fonts/source-code-pro
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

    (dynamic-fonts-setup)))


;;; The mode line

;; Standard stuff
(line-number-mode)
(column-number-mode)

(use-package anzu                       ; Position/matches count for isearch
  :ensure t
  :init (global-anzu-mode +1)
  :config
  (progn
    (set-face-attribute 'anzu-mode-line nil
                        :foreground "yellow" :weight 'bold)
    (custom-set-variables
     '(anzu-mode-lighter "")
     '(anzu-deactivate-region t)
     '(anzu-search-threshold 1000)
     '(anzu-replace-to-string-separator " => "))
    (global-set-key (kbd "M-%") 'anzu-query-replace)
    (global-set-key (kbd "C-M-%") 'anzu-query-replace-regexp)))


;;; The minibuffer
(setq history-length 1000)              ; Store more history

(use-package helm
  ;; :ensure t
  :load-path "~/wip/emacs-helm"         ; Use Git version
  :init
  (progn
    (require 'helm-config)
    (bind-key "C-c h" 'helm-command-prefix)
    (unbind-key "C-x c")
    (helm-mode))
  :bind (("C-x b" . helm-mini)
         ("M-l" . helm-buffers-list)
         ("M-y" . helm-show-kill-ring)
         ("M-x" . helm-M-x)
         ("C-c h o" . helm-occur)
         ("C-c h s" . helm-swoop)
         ("C-h a" . helm-apropos))
  :diminish helm-mode)


;;; Buffer, Windows and Frames
(use-package popwin
  :ensure t
  :init (popwin-mode 1))

(setq frame-resize-pixelwise t          ; Resize by pixels
      frame-title-format
      '(:eval (if (buffer-file-name)
                  (abbreviate-file-name (buffer-file-name)) "%b")))

(use-package frame
  :bind (("C-c T F" . toggle-frame-fullscreen))
  :init
  (progn
    ;; Kill `suspend-frame'
    (global-set-key (kbd "C-z") nil)
    (global-set-key (kbd "C-x C-z") nil))
  :config (add-to-list 'initial-frame-alist '(fullscreen . maximized)))

(use-package uniquify                   ; Make buffer names unique
  :config (setq uniquify-buffer-name-style 'forward))

(use-package ace-window
  :ensure t
  :bind (("M-p" . ace-window)))

(use-package desktop                    ; Save buffers, windows and frames
  :init (desktop-save-mode)
  :config (progn
            ;; Don't autosave desktops, it's too expensive.  Desktops aren't
            ;; that precious, and Emacs will save the desktop on exit anyway.
            (setq desktop-auto-save-timeout nil)

            (dolist (mode '(magit-mode git-commit-mode))
              (add-to-list 'desktop-modes-not-to-save mode))))

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
  ;; Use GNU ls for Emacs
  (when-let (gnu-ls (and (eq system-type 'darwin) (executable-find "gls")))
    (setq insert-directory-program gnu-ls)))

(use-package dired-x                    ; Additional tools for Dired
  :defer t
  :config
  (progn
    (setq dired-omit-verbose nil)        ; Shut up, dired

    (when (eq system-type 'darwin)
      ;; OS X bsdtar is mostly compatible with GNU Tar
      (setq dired-guess-shell-gnutar "tar")))
  :diminish ((dired-omit-mode . " ●")))

(use-package direx
  :ensure t
  :bind ("C-x C-j" . direx:jump-to-directory-other-window)
  :config
  (push '(direx:direx-mode :position left :width 25 :dedicated t)
        popwin:special-display-config))

(use-package ignoramus                  ; Ignore uninteresting files everywhere
  :ensure t
  :defer t
  :idle (ignoramus-setup))

(use-package recentf                    ; Save recently visited files
  :defer t
  :idle (recentf-mode)
  :config
  (setq recentf-max-saved-items 200
        recentf-max-menu-items 15
        ;; Cleanup recent files only when Emacs is idle, but not when the mode
        ;; is enabled, because that unnecessarily slows down Emacs. My Emacs
        ;; idles often enough to have the recent files list clean up regularly
        recentf-auto-cleanup 300
        recentf-exclude (list "/\\.git/.*\\'" ; Git contents
                              "/elpa/.*\\'" ; Package files
                              "/itsalltext/" ; It's all text temp files
                              ;; And all other kinds of boring files
                              #'ignoramus-boring-p)))

(use-package saveplace                  ; Save point position in files
  :config (setq-default save-place t))

(use-package autorevert                 ; Auto-revert buffers of changed files
  :init (global-auto-revert-mode))


;;; Basic editing

;; Disable tabs, but given them proper width
(setq-default indent-tabs-mode nil
              tab-width 8)

(use-package electric                   ; Electric code layout
  :init (electric-layout-mode))

(use-package elec-pair                  ; Electric pairs
  :init (electric-pair-mode))

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
(add-hook 'text-mode-hook #'auto-fill-mode)

(use-package whitespace-cleanup-mode    ; Cleanup whitespace in buffers
  :ensure t
  :bind (("C-c T W" . whitespace-cleanup-mode))
  :init (dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook))
          (add-hook hook #'whitespace-cleanup-mode))
  :diminish whitespace-cleanup-mode)


(use-package align                      ; Align text in buffers
  :bind (("C-c A a" . align)
         ("C-c A c" . align-current)
         ("C-c A r" . align-regexp)))

(use-package expand-region              ; Expand region by semantic units
  :ensure t
  :bind (("C-=" . er/expand-region)))

(use-package nlinum                     ; Line numbers in display margin
  :ensure t
  :bind (("C-c T l" . nlinum-mode)))


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
  :ensure t
  :bind (("C-c SPC" . ace-jump-mode)
         ("C-c j"   . ace-jump-mode)
         ("C-c J"   . ace-jump-mode-pop-mark))
  :config
  ;; Sync marks with Emacs built-in commands
  (ace-jump-mode-enable-mark-sync))

(use-package page-break-lines           ; Turn page breaks into lines
  :ensure t
  :init (global-page-break-lines-mode)
  :diminish page-break-lines-mode)


;;; Search
(use-package isearch                    ; Search buffers
  :bind (("C-c s s" . isearch-forward-symbol-at-point)))


;;; Highlights
(use-package whitespace                 ; Highlight bad whitespace
  :bind (("C-c T w" . whitespace-mode))
  :init
  (dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook))
    (add-hook hook #'whitespace-mode))
  :config
  ;; Highlight tabs, empty lines at beg/end, trailing whitespaces and overlong
  ;; portions of lines via faces.  Also indicate tabs via characters
  (setq whitespace-style '(face indentation space-after-tab space-before-tab
                                tab-mark empty trailing lines-tail)
        whitespace-line-column nil)     ; Use `fill-column' for overlong lines
  :diminish whitespace-mode)

(use-package hl-line                    ; Highlight the current line
  :init (global-hl-line-mode 1))

(use-package paren                      ; Highlight paired delimiters
  :init (show-paren-mode)
  :config (setq show-paren-when-point-inside-paren t
                show-paren-when-point-in-periphery t))

(use-package rainbow-delimiters         ; Highlight delimiters by depth
  :ensure t
  :defer t
  :init (dolist (hook '(text-mode-hook prog-mode-hook))
          (add-hook hook #'rainbow-delimiters-mode)))


;;; Skeletons, completion and expansion
(use-package company                    ; Graphical (auto-)completion
  ;; :ensure t
  :load-path "~/wip/company-mode"
  :defer t
  :idle (global-company-mode)
  :config
  (progn
    ;; Use Company for completion
    (bind-key [remap completion-at-point] #'company-complete company-mode-map)

    (setq company-tooltip-align-annotations t
          ;; Easy navigation to candidates with M-<n>
          company-show-numbers t))
  :diminish company-mode)

(use-package company-quickhelp          ; Documentation popups for Company
  :disabled t
  :ensure t
  :defer t
  :init (add-hook 'global-company-mode-hook #'company-quickhelp-mode))


;;; Spelling and syntax checking
(use-package ispell                     ; Spell checking
  :disabled t
  :defer t
  :config
  (progn
    (setq ispell-program-name (if (eq system-type 'darwin)
                                  (executable-find "aspell")
                                (executable-find "hunspell"))
          ispell-dictionary "en_GB"     ; Default dictionnary
          ispell-silently-savep t       ; Don't ask when saving the private dict
          ;; Increase the height of the choices window to take our header line
          ;; into account.
          ispell-choices-win-default-height 5)

    (unless ispell-program-name
      (warn "No spell checker available.  Install Hunspell or ASpell for OS X."))))

(use-package flyspell                   ; On-the-fly spell checking
  :disabled t
  :bind (("C-c T s" . flyspell-mode))
  :init
  (progn
    (dolist (hook '(text-mode-hook message-mode-hook))
      (add-hook hook 'turn-on-flyspell))
    (add-hook 'prog-mode-hook 'flyspell-prog-mode))
  :config
  (progn
    (setq flyspell-use-meta-tab nil
          ;; Make Flyspell less chatty
          flyspell-issue-welcome-flag nil
          flyspell-issue-message-flag nil)

    ;; Free C-M-i for completion
    (define-key flyspell-mode-map "\M-\t" nil))
  :diminish flyspell-mode)

(use-package flycheck                   ; On-the-fly syntax checking
  :ensure t
  :bind (("C-c l e" . list-flycheck-errors)
         ("C-c T f" . flycheck-mode))
  ;; :init (global-flycheck-mode)
  :diminish flycheck-mode)

(use-package flycheck-pos-tip           ; Show Flycheck messages in popups
  :disabled t
  :ensure t
  :defer t
  :init
  (setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages))


;;; Text editing
;; (use-package typo
;;   :ensure t
;;   :bind (("C-c T t" . typo-mode))
;;   :idle (typo-global-mode -1)
;;   :init (dolist (hook '(markdown-mode-hook
;;                         rst-mode-hook))
;;           (add-hook hook 'typo-mode)))


;;; Other markup languages
(use-package markdown-mode              ; Markdown
  :ensure t
  ;; Use GFM Mode for Markdown files from It's All Text.  It's better for most
  ;; sites to not add hard line breaks to content.
  :mode ("/itsalltext/.*\\.md\\'" . gfm-mode))

(use-package yaml-mode                  ; YAML
  :ensure t
  :defer t
  :config
  (add-hook 'yaml-mode-hook (lambda () (run-hooks 'prog-mode-hook))))


;;; Programming utilities
(use-package compile                    ; Compile from Emacs
  :bind (("C-c c" . compile)
         ("C-c C" . recompile))
  :config
  (progn
    (setq compilation-ask-about-save nil ; Just save before compiling
          compilation-always-kill t     ; Just kill old compile processes before
                                        ; starting the new one
          compilation-scroll-output 'first-error ; Automatically scroll to first
                                        ; error
          )))


;;; Generic Lisp
(use-package paredit                    ; Balanced sexp editing
  :ensure t
  :defer t
  :init
  (progn
    (dolist (hook '(eval-expression-minibuffer-setup-hook
                    emacs-lisp-mode-hook
                    inferior-emacs-lisp-mode-hook
                    clojure-mode-hook))
      (add-hook hook #'paredit-mode)))
  ;; :config
  ;; (progn
  ;;   ;; Free M-s.  There are some useful bindings in that prefix map.
  ;;   (define-key paredit-mode-map (kbd "M-s") nil)
  ;;   (define-key paredit-mode-map (kbd "M-S-<up>") #'paredit-splice-sexp))
  :diminish paredit-mode)


;;; Emacs Lisp
(use-package elisp-slime-nav            ; Jump to definition of symbol at point
  :ensure t
  :defer t
  :init (add-hook 'emacs-lisp-mode-hook #'elisp-slime-nav-mode)
  :diminish elisp-slime-nav-mode)

(use-package flycheck-package           ; Check package conventions with Flycheck
  :ensure t
  :defer t
  :init (with-eval-after-load 'flycheck (flycheck-package-setup)))

(use-package ielm                       ; Emacs Lisp REPL
  :bind (("C-c u z" . ielm)))

(bind-key "C-c T d" #'toggle-debug-on-error)


;;; Version control
(use-package diff-hl                    ; Highlight hunks in fringe
  :ensure t
  :defer t
  :init
  (progn
    ;; Highlight changes to the current file in the fringe
    (global-diff-hl-mode)
    ;; Highlight changed files in the fringe of Dired
    (add-hook 'dired-mode-hook 'diff-hl-dired-mode)

    ;; Fall back to the display margin, if the fringe is unavailable
    (unless (display-graphic-p)
      (diff-hl-margin-mode))))

(use-package magit                      ; The one and only Git frontend
  :ensure t
  :bind ("C-x g"   . magit-status)
  :config
  (progn
    ;; Shut up, Magit!
    (setq magit-save-some-buffers 'dontask
          magit-stage-all-confirm nil
          magit-unstage-all-confirm nil
          ;; Except when you ask something useful…
          magit-set-upstream-on-push t
          ;; Use IDO for completion
          magit-completing-read-function #'magit-ido-completing-read)

    ;; Auto-revert files after Magit operations
    (magit-auto-revert-mode))
  :diminish magit-auto-revert-mode)


;;; Tools and utilities
(use-package projectile                 ; Project management
  :ensure t
  :defer t
  :init (projectile-global-mode)
  :diminish projectile-mode)

(use-package helm-projectile
  :ensure t
  :defer t
  :init (helm-projectile-on)
  :config
  (setq projectile-completion-system 'helm))

(use-package paradox                    ; Better package menu
  :ensure t)


;;; Net & Web & Email
(use-package weibo
  ;; :ensure t
  :load-path "~/repos/weibo.emacs"
  :config
  (setq weibo-consumer-key "3426280940"
        weibo-consumer-secret "9de89c9ef2caf54fc32246885a33bcb4"))

(use-package sx                         ; StackExchange client for Emacs
  :ensure t
  :bind (("C-c w s" . sx-tab-frontpage)
         ("C-c w S" . sx-tab-newest)
         ("C-c w a" . sx-ask)))

(use-package mu4e
  :load-path "/usr/local/share/emacs/site-lisp/mu4e"
  :config
  (progn
    (setq mu4e-maildir "~/Maildir")
    (setq mu4e-drafts-folder "/[Gmail].Drafts")
    (setq mu4e-sent-folder   "/[Gmail].Sent Mail")
    (setq mu4e-trash-folder  "/[Gmail].Trash")
    (setq mu4e-maildir-shortcuts '(("/INBOX"               . ?i)
                                   ("/[Gmail].Sent Mail"   . ?s)
                                   ("/[Gmail].Starred"     . ?r)
                                   ("/[Gmail].Trash"       . ?t)
                                   ("/[Gmail].All Mail"    . ?a)))
    ;; allow for updating mail using 'U' in the main view:
    (setq mu4e-get-mail-command "proxychains4 offlineimap"
          mu4e-update-interval (* 18 60) ;; update every 30 minutes
          )
    ;; something about ourselves
    (setq user-mail-address "xuchunyang56@gmail.com"
          user-full-name  "Chunyang Xu"
          mu4e-compose-signature "Chunyang Xu")
    (setq mu4e-headers-skip-duplicates t)
    (require 'org-mu4e)))


;;; Dictionary
(use-package youdao-dictionary
  :ensure t
  :bind (("C-c y" . youdao-dictionary-search-at-point))
  :config
  (push "*Youdao Dictionary*" popwin:special-display-config))

(use-package osx-dictionary
  :ensure t
  :bind (("C-c d" . osx-dictionary-search-pointer))
  :config
  (push "*osx-dictionary*" popwin:special-display-config))


;;; Org-mode
(use-package org
  :bind (("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         ("C-c L" . org-store-link))
  :config
  (progn
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
                                   (scheme . t)))))

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; init.el ends here
