;;; init.el --- Emacs configuration of Chunyang Xu -*- lexical-binding: t; -*-

;;; Copyright (C) 2015 Chunyang Xu <xuchunyang56@gmail.com>
;;
;; Author: Chunyang Xu <xuchunyang56@gmail.com>
;; URL: https://gihub.com/xuchunyang/my-emacs-config
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

;; TODO:
;; - [ ] send email (proxy problem, use external tool to handle this)

;;; Code:

;;; Debugging
(setq message-log-max 10000)

;; (setq debug-on-error t)

(defconst emacs-start-time (current-time))

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
(defconst chunyang-custom-file (locate-user-emacs-file "custom.el")
  "File used to store settings from Customization UI.")

(use-package cus-edit
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
  :if (eq system-type 'darwin)
  :config
  (setq ns-pop-up-frames nil            ; Don't pop up new frames from the workspace
        mac-command-modifier 'meta
        mac-option-modifier 'control))

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

(use-package lunaryorn-scratch          ; My logo in the scratch buffer
  :disabled t
  :commands (lunaryorn-insert-logo
             lunaryorn-insert-logo-into-scratch)
  :config
  (add-hook 'after-init-hook #'lunaryorn-insert-logo-into-scratch))

(use-package dynamic-fonts              ; Select best available font
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

(use-package unicode-fonts              ; Map Unicode blocks to fonts
  :disabled t
  :ensure t
  :config (unicode-fonts-setup))

(use-package zenburn-theme
  :disabled nil
  :ensure t
  :init (load-theme 'zenburn t))


;;; The mode line

(setq-default header-line-format
              '(which-func-mode ("" which-func-format " "))
              mode-line-format
              '("%e" mode-line-front-space
                ;; Standard info about the current buffer
                mode-line-mule-info
                mode-line-client
                mode-line-modified
                mode-line-remote
                mode-line-frame-identification
                mode-line-buffer-identification " " mode-line-position
                ;; Some specific information about the current buffer:
                ;; - Paredit
                ;; (paredit-mode (:propertize " ()" face bold))
                (projectile-mode projectile-mode-line)
                (vc-mode vc-mode)
                (flycheck-mode flycheck-mode-line) ; Flycheck status
                ;; (anzu-mode (:eval                  ; isearch pos/matches
                ;;             (when (> anzu--total-matched 0)
                ;;               (concat " " (anzu--update-mode-line)))))
                (multiple-cursors-mode mc/mode-line) ; Number of cursors
                ;; And the modes, which we don't really care for anyway
                " " mode-line-misc-info mode-line-modes mode-line-end-spaces)
              mode-line-remote
              '(:eval
                (when-let (host (file-remote-p default-directory 'host))
                  (propertize (concat "@" host) 'face
                              '(italic warning))))
              ;; Remove which func from the mode line, since we have it in the
              ;; header line
              mode-line-misc-info
              (assq-delete-all 'which-func-mode mode-line-misc-info))

;; Standard stuff
(line-number-mode)
(column-number-mode)

(use-package anzu                       ; Position/matches count for isearch
  :ensure t
  :bind (("M-%"   . anzu-query-replace)
         ("C-M-%" . anzu-query-replace-regexp))
  :config
  (global-anzu-mode)
  (set-face-attribute 'anzu-mode-line nil
                      :foreground "yellow" :weight 'bold)
  (custom-set-variables
   '(anzu-mode-lighter "")
   '(anzu-deactivate-region t)
   '(anzu-search-threshold 1000)
   '(anzu-replace-to-string-separator " => "))
  :diminish anzu-mode)

(use-package which-func                 ; Current function name in header line
  :config
  (which-function-mode)
  (setq which-func-unknown "⊥" ; The default is really boring…
        which-func-format
        `((:propertize (" ➤ " which-func-current)
                       local-map ,which-func-keymap
                       face which-func
                       mouse-face mode-line-highlight
                       help-echo "mouse-1: go to beginning\n\
mouse-2: toggle rest visibility\n\
mouse-3: go to end"))))

(use-package fancy-battery              ; Fancy battery info for mode line
  :disabled t
  :ensure t)


;;; The minibuffer
(setq history-length 1000)              ; Store more history

(use-package helm
  ;; :load-path "~/wip/emacs-helm"
  :ensure t
  :diminish helm-mode
  :config
  (use-package helm-config)
  (unbind-key "C-x c")
  (bind-keys ("C-c h"   . helm-command-prefix)
             ("C-x b"   . helm-mini)
             ("C-x f"   . helm-recentf)
             ("C-x C-f" . helm-find-files)
             ("M-l"     . helm-buffers-list)
             ("M-y"     . helm-show-kill-ring)
             ("M-x"     . helm-M-x)
             ("C-c h o" . helm-occur))
  (helm-mode))

(use-package ido
  :disabled t
  :config
  (ido-mode 1)
  (use-package ido-vertical-mode
    :ensure t
    :config
    (ido-vertical-mode)
    (setq ido-enable-prefix nil
          ido-enable-flex-matching t
          ido-case-fold t                       ;; ignore case
          ido-auto-merge-work-directories-length -1 ;; disable auto-merge (it's confusing)
          ido-create-new-buffer 'always             ;; create new files easily
          ido-use-filename-at-point nil ;; don't try to be smart about what I want
          )
    ;; I like visual matching (colors)
    (setq ido-use-faces t)))

(use-package smex
  :disabled t
  :ensure t
  :config
  (smex-initialize)
  (global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "M-X") 'smex-major-mode-commands)
  ;; This is your old M-x.
  (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command))


;;; Buffer, Windows and Frames

(setq frame-resize-pixelwise t          ; Resize by pixels
      frame-title-format
      '(:eval (if (buffer-file-name)
                  (abbreviate-file-name (buffer-file-name)) "%b")))

(use-package popwin
  :ensure t
  :config (popwin-mode 1))

(use-package frame
  :bind (("C-c T F" . toggle-frame-fullscreen))
  :config
  ;; Kill `suspend-frame'
  (global-set-key (kbd "C-z") nil)
  (global-set-key (kbd "C-x C-z") nil)
  (add-to-list 'initial-frame-alist '(fullscreen . maximized)))

(use-package uniquify                   ; Make buffer names unique
  :config (setq uniquify-buffer-name-style 'forward))

(use-package ibuffer                    ; Better buffer list
  :bind (([remap list-buffers] . ibuffer)))

(use-package ace-window
  :ensure t
  :bind (("M-o" . ace-window)))

(use-package desktop                    ; Save buffers, windows and frames
  :config
  (desktop-save-mode))

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

(use-package dired                      ; Edit directories
  :config
  (use-package dired-x
    :diminish dired-omit-mode
    :config
    (setq dired-omit-files (concat dired-omit-files "\\|^\\..+$"))
    (add-hook 'dired-mode-hook (lambda () (dired-omit-mode)))))

(use-package direx
  :disabled t
  :ensure t
  :bind (("C-x C-j" . direx:jump-to-directory-other-window))
  :config
  (push '(direx:direx-mode :position left :width 25 :dedicated t)
        popwin:special-display-config))

(use-package neotree
  :disabled t
  :ensure t
  :bind (("C-x C-j" . neotree-toggle)))

(use-package bookmark                   ; Bookmarks for Emacs buffers
  :bind (("C-c l b" . list-bookmarks))
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
                              ;; And all other kinds of boring files
                              #'ignoramus-boring-p))
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
(add-hook 'text-mode-hook #'auto-fill-mode)

(use-package chunyang-simple
  :load-path "personal"
  :bind (([remap split-window-right] . chunyang-split-window-right)))

(use-package whitespace-cleanup-mode    ; Cleanup whitespace in buffers
  :ensure t
  :bind (("C-c T W" . whitespace-cleanup-mode))
  :config (dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook))
          (add-hook hook #'whitespace-cleanup-mode))
  :diminish whitespace-cleanup-mode)

(use-package subword                    ; Subword/superword editing
  :diminish subword-mode)

(use-package adaptive-wrap              ; Choose wrap prefix automatically
  :ensure t
  :config (add-hook 'visual-line-mode-hook #'adaptive-wrap-prefix-mode))

(use-package visual-fill-column
  :ensure t
  ;; Keep the fringe
  :config
  (setq visual-fill-column-disable-fringe nil)
  (add-hook 'visual-line-mode-hook #'visual-fill-column-mode))

(use-package visual-regexp              ; Regexp replace with in-buffer display
  :disabled t
  :ensure t
  :bind (("C-c r" . vr/query-replace)
         ("C-c R" . vr/replace)))

(use-package zop-to-char
  :disabled t
  :ensure t
  :bind (("M-z" . zop-to-char)
         ("M-Z" . zop-up-to-char)))

(use-package easy-kill                  ; Easy killing and marking on C-w
  :ensure t
  :bind (([remap kill-ring-save] . easy-kill)
         ([remap mark-sexp]      . easy-mark)))

(use-package align                      ; Align text in buffers
  :bind (("C-c A a" . align)
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
  :disabled t
  :commands undo-tree-mode
  :config (global-undo-tree-mode)
  :diminish undo-tree-mode)

(use-package nlinum                     ; Line numbers in display margin
  :ensure t
  :bind (("C-c n l" . nlinum-mode)))

;; Give us narrowing back!
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)


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
  :config (global-page-break-lines-mode)
  :diminish page-break-lines-mode)

(use-package outline                    ; Navigate outlines in buffers
  :config (dolist (hook '(text-mode-hook prog-mode-hook))
          (add-hook hook #'outline-minor-mode))
  :diminish outline-minor-mode)

(use-package imenu-anywhere             ; Helm-based imenu across open buffers
  :ensure t
  :bind (("C-c i" . helm-imenu-anywhere)))


;;; Search
(use-package isearch                    ; Search buffers
  :bind (("C-c s s" . isearch-forward-symbol-at-point)))

(use-package helm-swoop
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
(use-package whitespace                 ; Highlight bad whitespace
  :bind (("C-c T w" . whitespace-mode))
  :config
  (dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook))
    (add-hook hook #'whitespace-mode))
  ;; Highlight tabs, empty lines at beg/end, trailing whitespaces and overlong
  ;; portions of lines via faces.  Also indicate tabs via characters
  (setq whitespace-style '(face indentation space-after-tab space-before-tab
                                tab-mark empty trailing lines-tail)
        whitespace-line-column nil)     ; Use `fill-column' for overlong lines
  :diminish whitespace-mode)

(use-package hl-line
  :commands hl-line-mode
  :bind (("C-c C-l" . hl-line-mode))
  :config
  (use-package hl-line+
    :ensure t))

(use-package paren                      ; Highlight paired delimiters
  :config
  (show-paren-mode 1))

(use-package rainbow-delimiters         ; Highlight delimiters by depth
  :ensure t
  :config (dolist (hook '(text-mode-hook prog-mode-hook))
            (add-hook hook #'rainbow-delimiters-mode)))


;;; Skeletons, completion and expansion

;; In `completion-at-point', do not pop up silly completion buffers for less
;; than five candidates.  Cycle instead.
(setq completion-cycle-threshold 5)

(use-package hippie-exp                 ; Powerful expansion and completion
  :bind (([remap dabbrev-expand] . hippie-expand))
  :config
  (setq hippie-expand-try-functions-list
        '(try-expand-dabbrev
          try-expand-dabbrev-all-buffers
          try-expand-dabbrev-from-kill
          try-complete-file-name-partially
          try-complete-file-name
          try-expand-all-abbrevs
          try-expand-list
          try-complete-lisp-symbol-partially
          try-complete-lisp-symbol)))

(use-package company                    ; Graphical (auto-)completion
  :ensure t
  :diminish company-mode
  :config
  ;; Use Company for completion
  (bind-key [remap completion-at-point] #'company-complete company-mode-map)
  (setq company-tooltip-align-annotations t
        ;; Easy navigation to candidates with M-<n>
        company-show-numbers t)
  (global-company-mode))

(use-package company-quickhelp          ; Documentation popups for Company
  :disabled t
  :ensure t
  :config (add-hook 'global-company-mode-hook #'company-quickhelp-mode))

;; (require 'auto-complete-config)
;; (ac-config-default)


;;; Spelling and syntax checking
(use-package flyspell
  :diminish flyspell-mode
  :bind (("C-c n s" . flyspell-mode)
         ("C-c n c" . flyspell-prog-mode)))

(use-package flycheck
  :ensure t
  :diminish flycheck-mode
  :bind (("C-c n f" . global-flycheck-mode)
         ("C-c l e" . list-flycheck-errors))
  ;; :config
  ;; (setq flycheck-emacs-lisp-load-path 'inherit)
  )

(use-package flycheck-pos-tip           ; Show Flycheck messages in popups
  :ensure t
  :config
  (setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages))



;;; Text editing
(use-package typo
  :disabled t
  :ensure t)


;;; Other markup languages
(use-package markdown-mode
  :ensure t
  :mode (("\\`README\\.md\\'" . gfm-mode)
         ("\\.md\\'"          . markdown-mode)
         ("\\.markdown\\'"    . markdown-mode)))

(use-package yaml-mode                  ; YAML
  :ensure t
  :config
  (add-hook 'yaml-mode-hook (lambda () (run-hooks 'prog-mode-hook))))


;;; Programming utilities
(use-package compile                    ; Compile from Emacs
  :bind (("C-c c" . compile)
         ("C-c C" . recompile))
  :config
  (setq compilation-ask-about-save nil  ; Just save before compiling
        compilation-always-kill t       ; Just kill old compile processes before
                                        ; starting the new one
        compilation-scroll-output 'first-error ; Automatically scroll to first
                                        ; error
        ))

(use-package highlight-numbers          ; Fontify number literals
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'highlight-numbers-mode))

(use-package highlight-symbol           ; Highlighting and commands for symbols
  :ensure t
  :defer 15
  :bind
  (("C-c s %" . highlight-symbol-query-replace)
   ("C-c s n" . highlight-symbol-next-in-defun)
   ("C-c s o" . highlight-symbol-occur)
   ("C-c s p" . highlight-symbol-prev-in-defun))
  :config
  (setq highlight-symbol-idle-delay 0.4 ; Highlight almost immediately
        highlight-symbol-on-navigation-p t)
  ;; Navigate occurrences of the symbol under point with M-n and M-p
  (add-hook 'prog-mode-hook #'highlight-symbol-nav-mode)
  ;; Highlight symbol occurrences
  (add-hook 'prog-mode-hook #'highlight-symbol-mode)
  :diminish highlight-symbol-mode)

(use-package rainbow-mode               ; Fontify color values in code
  :ensure t
  :bind (("C-c T r" . rainbow-mode))
  :config (add-hook 'css-mode-hook #'rainbow-mode))


;;; Generic Lisp
(use-package paredit                    ; Balanced sexp editing
  :ensure t
  :diminish paredit-mode
  :config
  (dolist (hook '(eval-expression-minibuffer-setup-hook
                  emacs-lisp-mode-hook
                  inferior-emacs-lisp-mode-hook
                  clojure-mode-hook))
    (add-hook hook #'paredit-mode))
  ;; Free M-s.  There are some useful bindings in that prefix map.
  (define-key paredit-mode-map (kbd "M-s") nil)
  (define-key paredit-mode-map (kbd "M-s M-s") #'paredit-splice-sexp)
  (unbind-key "C-j" paredit-mode-map))


;;; Emacs Lisp
(use-package elisp-slime-nav            ; Jump to definition of symbol at point
  :disabled t
  :ensure t
  :config (add-hook 'emacs-lisp-mode-hook #'elisp-slime-nav-mode)
  :diminish elisp-slime-nav-mode)

(use-package flycheck-package           ; Check package conventions with Flycheck
  :ensure t
  :config (with-eval-after-load 'flycheck (flycheck-package-setup)))

(use-package ielm                       ; Emacs Lisp REPL
  :bind (("C-c u z" . ielm)))

(bind-key "C-c T d" #'toggle-debug-on-error)


;;; Version control
(use-package diff-hl                    ; Highlight hunks in fringe
  :ensure t
  :config
  ;; Highlight changes to the current file in the fringe
  (global-diff-hl-mode)
  ;; Highlight changed files in the fringe of Dired
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode)

  ;; Fall back to the display margin, if the fringe is unavailable
  (unless (display-graphic-p)
    (diff-hl-margin-mode)))

(use-package magit                      ; The one and only Git frontend
  :ensure t
  :bind ("C-x g" . magit-status)
  :diminish magit-auto-revert-mode)

(use-package git-commit-mode            ; Git commit message mode
  :ensure t)

(use-package gitconfig-mode             ; Git configuration mode
  :ensure t)

(use-package gitignore-mode             ; .gitignore mode
  :ensure t)

(use-package git-timemachine            ; Go back in Git time
  :disabled t
  :ensure t
  :bind (("C-c v t" . git-timemachine)))


;;; Tools and utilities
(use-package projectile                 ; Project management
  :ensure t
  :config (projectile-global-mode)
  :diminish projectile-mode)

(use-package helm-projectile
  :ensure t
  :config
  (setq projectile-completion-system 'helm)
  (helm-projectile-on)
  (use-package helm-ag
    :bind (("M-s s" . helm-projectile-ag))
    :ensure t)
  (use-package helm-ack
    :ensure t))

(use-package helm-open-github
  :ensure t
  :defer 30
  :bind (("C-c o i" . helm-open-github-from-issues)
         ("C-c o f" . helm-open-github-from-file)
         ("C-c o c" . helm-open-github-from-commit)
         ("C-c o p" . helm-open-github-from-pull-requests)))

(use-package helm-github-stars
  :defer 20
  :load-path "~/wip/helm-github-stars"
  :config (setq helm-github-stars-username "xuchunyang"
                helm-github-stars-name-length 30
                helm-github-stars-cache-file "~/.emacs.d/var/hgs-cache"))

(use-package paradox                    ; Better package menu
  :disabled t
  :ensure t
  :defer 20
  :bind (("C-c l p" . paradox-list-packages)
         ("C-c l P" . package-list-packages-no-fetch))
  :config
  (setq paradox-github-token t
        paradox-execute-asynchronously nil))

(use-package guide-key
  :ensure t
  :diminish guide-key-mode
  :commands guide-key-mode
  :defer 10
  :config
  (setq guide-key/guide-key-sequence '("C-x r" "C-x 4" "C-c h"))
  (guide-key-mode 1))

(use-package hydra
  :ensure t
  :disabled t)

(use-package chinese-pyim
  :disabled t
  :load-path "~/wip/chinese-pyim"
  :init
  (require 'chinese-pyim)
  (setq default-input-method "chinese-pyim")
  (setq pyim-dicts
        '((:name "dict1" :file "~/wip/chinese-pyim/pyim-bigdict.txt" :coding utf-8-unix))))


;;; Net & Web & Email
(use-package eww                        ; Emacs' built-in web browser
  :disabled t
  :bind (("C-c w b" . eww-list-bookmarks)
         ("C-c w W" . eww)))

(use-package circe
  :ensure t
  :config
  (load-file  "~/.private.el")
  (setq circe-network-options
        `(("Freenode"
           :nick "chunyang"
           :channels ("#emacs", "#MacPorts")
           :nickserv-password ,freenode-password))))

;; (add-to-list 'load-path "~/wip/weibo.el/")
;; (require 'weibo)
;; (load-file "~/.private.el")

;; (use-package weibo
;;   :disabled nil
;;   :load-path "~/wip/weibo.el/"
;;   :defer 30
;;   :config
;;   (require 'weibo)
;;   (load-file "~/.private.el"))

(use-package google-this
  :ensure t
  :diminish google-this-mode
  :bind (("C-c g" . google-this-mode-submap)))

(use-package sx                         ; StackExchange client for Emacs
  :disabled t
  :ensure t
  :bind (("C-c w s" . sx-tab-frontpage)
         ("C-c w S" . sx-tab-newest)
         ("C-c w a" . sx-ask)))

(use-package mu4e
  :disabled t
  :load-path "lisp/mu4e/"
  :defer 15
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
  :disabled t
  :load-path "/opt/local/share/emacs/site-lisp"
  :config
  (setq notmuch-saved-searches
        '((:name "inbox" :query "tag:inbox")
          (:name "unread" :query "tag:inbox AND tag:unread"))))

;;; Dictionary
(use-package youdao-dictionary
  :ensure t
  :defer 10
  :bind (("C-c y" . youdao-dictionary-search-at-point))
  :config
  (setq url-automatic-caching t)
  (push "*Youdao Dictionary*" popwin:special-display-config))

(use-package osx-dictionary
  :ensure t
  :defer 10
  :bind (("C-c d" . osx-dictionary-search-pointer))
  :config
  (push "*osx-dictionary*" popwin:special-display-config))

(use-package helm-dict
  :load-path "personal/"
  :defer 10
  ;; TODO: how to add new key to a existing prefix keymap?
  :bind ("M-4" . helm-dict))

(use-package langtool
  :ensure t
  :disabled t
  :config
  (setq
   langtool-language-tool-jar "~/Downloads/LanguageTool-2.8/languagetool-commandline.jar"
   setq) langtool-mother-tongue "en")


;;; Org-mode
(use-package org
  :bind (("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         ("C-c L" . org-store-link))
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

(bind-key "C-c h h" #'describe-personal-keybindings)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; init.el ends here
