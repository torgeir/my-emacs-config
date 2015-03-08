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

(use-package lunaryorn-osx              ; Personal OS X tools
  :if (eq system-type 'darwin)
  :load-path "personal/"
  :defines (lunaryorn-darwin-trash-tool)
  :init
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
  :init
  (add-hook 'after-init-hook #'lunaryorn-insert-logo-into-scratch))

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

(use-package unicode-fonts              ; Map Unicode blocks to fonts
  :ensure t
  :init (unicode-fonts-setup))

(use-package zenburn-theme
  :ensure t
  :defer t
  :init (load-theme 'zenburn t))


;;; The mode line

(setq-default ;; header-line-format
              ;; '(which-func-mode ("" which-func-format " "))
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
                ;; - Dired Omit Mode
                ;; (paredit-mode (:propertize " ()" face bold))
                ;; A little complicated, pending
                ;; https://github.com/rolandwalker/ignoramus/pull/3
                (dired-omit-mode (:eval (when (derived-mode-p 'dired-mode)
                                          " ●")))
                ;; Warn if whitespace isn't highlighted or cleaned in this
                ;; buffer.
                (:eval (unless buffer-read-only
                         (cond
                          ((not (bound-and-true-p whitespace-mode))
                           (propertize " SPACE" 'face '(bold error)))
                          ((not (bound-and-true-p whitespace-cleanup-mode))
                           (propertize " WSC" 'face 'warning)))))
                (projectile-mode projectile-mode-line)
                (vc-mode vc-mode)
                (flycheck-mode flycheck-mode-line) ; Flycheck status
                (anzu-mode (:eval                  ; isearch pos/matches
                            (when (> anzu--total-matched 0)
                              (concat " " (anzu--update-mode-line)))))
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
  :init (global-anzu-mode)
  :config (setq anzu-cons-mode-line-p nil)
  :diminish anzu-mode)

(use-package which-func                 ; Current function name in header line
  :disabled t
  :defer t
  :idle (which-function-mode)
  :idle-priority 1
  :config
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
  :ensure t
  :defer t
  :idle (fancy-battery-mode)
  :idle-priority 10)


;;; The minibuffer
(setq history-length 1000)              ; Store more history

(use-package helm
  :ensure t
  :diminish helm-mode
  :init
  (progn
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
    (helm-mode)))


;;; Buffer, Windows and Frames

(setq frame-resize-pixelwise t          ; Resize by pixels
      frame-title-format
      '(:eval (if (buffer-file-name)
                  (abbreviate-file-name (buffer-file-name)) "%b")))

(use-package popwin
  :ensure t
  :init (popwin-mode 1))

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

(use-package ibuffer                    ; Better buffer list
  :bind (([remap list-buffers] . ibuffer)))

(use-package ace-window
  :ensure t
  :bind (("M-o" . ace-window)))

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

(use-package dired                      ; Edit directories
  :defer t
  :config
  (progn
    (require 'dired-x)

    (setq dired-auto-revert-buffer t    ; Revert on re-visiting
          ;; Better dired flags: `-l' is mandatory, `-a' shows all files, `-h'
          ;; uses human-readable sizes, and `-F' appends file-type classifiers
          ;; to file names (for better highlighting)
          dired-listing-switches "-alhF"
          dired-ls-F-marks-symlinks t   ; -F marks links with @
          ;; Inhibit prompts for simple recursive operations
          dired-recursive-copies 'always)

    (when (or (memq system-type '(gnu gnu/linux))
              (string= (file-name-nondirectory insert-directory-program) "gls"))
      ;; If we are on a GNU system or have GNU ls, add some more `ls' switches:
      ;; `--group-directories-first' lists directories before files, and `-v'
      ;; sorts numbers in file names naturally, i.e. "image1" goes before
      ;; "image02"
      (setq dired-listing-switches
            (concat dired-listing-switches " --group-directories-first -v")))))

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

(use-package bookmark                   ; Bookmarks for Emacs buffers
  :bind (("C-c l b" . list-bookmarks))
  ;; Save bookmarks immediately after a bookmark was added
  :config (setq bookmark-save-flag 1))

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

(use-package launch                     ; Open files in external programs
  :ensure t
  :defer t
  :idle (global-launch-mode))

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

(use-package chunyang-simple
  :load-path "personal"
  :bind (([remap split-window-right] . chunyang-split-window-right)))

(use-package whitespace-cleanup-mode    ; Cleanup whitespace in buffers
  :ensure t
  :bind (("C-c T W" . whitespace-cleanup-mode))
  :init (dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook))
          (add-hook hook #'whitespace-cleanup-mode))
  :diminish whitespace-cleanup-mode)

(use-package subword                    ; Subword/superword editing
  :defer t
  :diminish subword-mode)

(use-package adaptive-wrap              ; Choose wrap prefix automatically
  :ensure t
  :defer t
  :init (add-hook 'visual-line-mode-hook #'adaptive-wrap-prefix-mode))

(use-package visual-fill-column
  :ensure t
  :defer t
  :init (add-hook 'visual-line-mode-hook #'visual-fill-column-mode)
  ;; Keep the fringe
  :config (setq visual-fill-column-disable-fringe nil))

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
  :disabled t
  :ensure t
  :bind (("C-=" . er/expand-region)))

(use-package undo-tree                  ; Branching undo
  :ensure t
  :init (global-undo-tree-mode)
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
  :init (global-page-break-lines-mode)
  :diminish page-break-lines-mode)

(use-package outline                    ; Navigate outlines in buffers
  :defer t
  :init (dolist (hook '(text-mode-hook prog-mode-hook))
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
  (progn
    ;; When doing isearch, hand the word over to helm-swoop
    (define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)
    ;; From helm-swoop to helm-multi-swoop-all
    (define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop)
    ;; Move up and down like isearch
    (define-key helm-swoop-map (kbd "C-r") 'helm-previous-line)
    (define-key helm-swoop-map (kbd "C-s") 'helm-next-line)
    (define-key helm-multi-swoop-map (kbd "C-r") 'helm-previous-line)
    (define-key helm-multi-swoop-map (kbd "C-s") 'helm-next-line)))

(use-package pinyin-search
  :ensure t)


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
  :disabled t
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
  :defer t
  :diminish company-mode
  :idle (global-company-mode)
  :config
  (progn
    ;; Use Company for completion
    (bind-key [remap completion-at-point] #'company-complete company-mode-map)
    (setq company-tooltip-align-annotations t
          ;; Easy navigation to candidates with M-<n>
          company-show-numbers t)))

(use-package company-quickhelp          ; Documentation popups for Company
  :disabled t
  :ensure t
  :defer t
  :init (add-hook 'global-company-mode-hook #'company-quickhelp-mode))


;;; Spelling and syntax checking
(use-package flyspell
  :diminish flyspell-mode
  :bind (("C-c n s" . flyspell-mode)
         ("C-c n c" . flyspell-prog-mode))
  :config (flyspell-mode 1))

(use-package flycheck
  :ensure t
  :diminish flycheck-mode
  :bind (("C-c n f" . global-flycheck-mode)
         ("C-c l e" . list-flycheck-errors))
  :init (global-flycheck-mode 1)
  :config
  (progn
    (setq flycheck-command-map 'ido)
    ;; Use italic face for checker name
    (set-face-attribute 'flycheck-error-list-checker-name nil :inherit 'italic)))

(use-package flycheck-pos-tip           ; Show Flycheck messages in popups
  :ensure t
  :defer t
  :init
  (setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages))


;;; Text editing
(use-package typo
  :disabled t
  :ensure t
  :bind (("C-c T t" . typo-mode))
  :idle (typo-global-mode -1)
  :init (dolist (hook '(markdown-mode-hook
                        rst-mode-hook))
          (add-hook hook 'typo-mode)))


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

(use-package highlight-numbers          ; Fontify number literals
  :ensure t
  :defer t
  :init
  (add-hook 'prog-mode-hook #'highlight-numbers-mode))

(use-package highlight-symbol           ; Highlighting and commands for symbols
  :ensure t
  :defer t
  :bind
  (("C-c s %" . highlight-symbol-query-replace)
   ("C-c s n" . highlight-symbol-next-in-defun)
   ("C-c s o" . highlight-symbol-occur)
   ("C-c s p" . highlight-symbol-prev-in-defun))
  :init
  (progn
    ;; Navigate occurrences of the symbol under point with M-n and M-p
    (add-hook 'prog-mode-hook #'highlight-symbol-nav-mode)
    ;; Highlight symbol occurrences
    (add-hook 'prog-mode-hook #'highlight-symbol-mode))
  :config
  (setq highlight-symbol-idle-delay 0.4     ; Highlight almost immediately
        highlight-symbol-on-navigation-p t) ; Highlight immediately after
                                        ; navigation
  :diminish highlight-symbol-mode)

(use-package rainbow-mode               ; Fontify color values in code
  :ensure t
  :bind (("C-c T r" . rainbow-mode))
  :config (add-hook 'css-mode-hook #'rainbow-mode))


;;; Generic Lisp
(use-package paredit                    ; Balanced sexp editing
  :ensure t
  :diminish paredit-mode
  :defer t
  :init
  (progn
    (dolist (hook '(eval-expression-minibuffer-setup-hook
                    emacs-lisp-mode-hook
                    inferior-emacs-lisp-mode-hook
                    clojure-mode-hook))
      (add-hook hook #'paredit-mode)))
  :config
  (progn
    ;; Free M-s.  There are some useful bindings in that prefix map.
    (define-key paredit-mode-map (kbd "M-s") nil)
    (define-key paredit-mode-map (kbd "M-s M-s") #'paredit-splice-sexp)
    (unbind-key "C-j" paredit-mode-map)))


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
  :defer t
  :bind ("C-x g" . magit-status)
  :config
  (progn
    ;; Shut up, Magit!
    (setq magit-save-some-buffers 'dontask
          magit-stage-all-confirm nil
          magit-unstage-all-confirm nil
          ;; Except when you ask something useful…
          magit-set-upstream-on-push t)

    ;; Auto-revert files after Magit operations
    (magit-auto-revert-mode))
  :diminish magit-auto-revert-mode)

(use-package git-commit-mode            ; Git commit message mode
  :ensure t
  :defer t)

(use-package gitconfig-mode             ; Git configuration mode
  :ensure t
  :defer t)

(use-package gitignore-mode             ; .gitignore mode
  :ensure t
  :defer t)

(use-package git-timemachine            ; Go back in Git time
  :ensure t
  :bind (("C-c v t" . git-timemachine)))


;;; Tools and utilities
(use-package projectile                 ; Project management
  :ensure t
  :defer t
  :init (projectile-global-mode)
  :diminish projectile-mode)

(use-package helm-projectile
  :ensure t
  :defer t
  :idle (helm-projectile-on)
  :config
  (progn
    (setq projectile-completion-system 'helm)
    (use-package helm-ag
      :ensure t)
    (use-package helm-ack
      :ensure t)))

(use-package helm-open-github
  :ensure t
  :bind (("C-c o i" . helm-open-github-from-issues)
         ("C-c o f" . helm-open-github-from-file)
         ("C-c o c" . helm-open-github-from-commit)
         ("C-c o p" . helm-open-github-from-pull-requests)))

(use-package paradox                    ; Better package menu
  :ensure t
  :bind (("C-c l p" . paradox-list-packages)
         ("C-c l P" . package-list-packages-no-fetch))
  :config
  (setq paradox-github-token t
        paradox-execute-asynchronously nil))

(use-package guide-key
  :ensure t
  :defer t
  :idle (guide-key-mode 1)
  :config
  (setq guide-key/guide-key-sequence '("C-x r" "C-x 4" "C-c h"))
  :diminish guide-key-mode)

(use-package hydra
  :ensure t
  :config
  (progn
    ;; Page navigation
    (defhydra hydra-page (ctl-x-map "" :pre (widen))
      "page"
      ("]" forward-page "next")
      ("[" backward-page "prev")
      ("n" narrow-to-page "narrow" :bind nil :exit t))
    ;; Goto Line
    (defhydra hydra-goto-line (goto-map ""
                                        :pre (linum-mode 1)
                                        :post (linum-mode -1))
      "goto-line"
      ("g" goto-line "go")
      ("m" set-mark-command "mark" :bind nil)
      ("q" nil "quit"))
    ;; Moving Text
    (use-package move-text
      :ensure t
      :config (move-text-default-bindings)) ; M-<up> M-<down> to move line up/down
    (defhydra hydra-move-text ()
      "Move text"
      ("u" move-text-up "up")
      ("d" move-text-down "down"))
    (defhydra hydra-window (:color pink)
      "
 Split: _v_ert _x_:horz
Delete: _o_nly  _da_ce  _dw_indow  _db_uffer  _df_rame
  Move: _s_wap
Frames: _f_rame new  _df_ delete
  Misc: _m_ark _a_ce  _u_ndo  _r_edo"
      ("h" windmove-left nil)
      ("j" windmove-down nil)
      ("k" windmove-up nil)
      ("l" windmove-right nil)
      ("H" hydra-move-splitter-left nil)
      ("J" hydra-move-splitter-down nil)
      ("K" hydra-move-splitter-up nil)
      ("L" hydra-move-splitter-right nil)
      ("|" (lambda ()
             (interactive)
             (split-window-right)
             (windmove-right)) nil)
      ("_" (lambda ()
             (interactive)
             (split-window-below)
             (windmove-down)) nil)
      ("v" split-window-right nil)
      ("x" split-window-below nil)
                                        ;("t" transpose-frame "'")
      ("u" winner-undo nil)
      ("r" winner-redo nil) ;;Fixme, not working?
      ("o" delete-other-windows nil :exit t)
      ("a" ace-window nil :exit t)
      ("f" new-frame nil :exit t)
      ("s" ace-swap-window nil)
      ("da" ace-delete-window nil)
      ("dw" delete-window nil)
      ("db" kill-this-buffer nil)
      ("df" delete-frame nil :exit t)
      ("q" nil nil)
                                        ;("i" ace-maximize-window "ace-one" :color blue)
                                        ;("b" ido-switch-buffer "buf")
      ("m" headlong-bookmark-jump nil))
    (bind-key "C-c w w" #'hydra-window/body)))


;;; Net & Web & Email
(use-package eww                        ; Emacs' built-in web browser
  :bind (("C-c w b" . eww-list-bookmarks)
         ("C-c w W" . eww)))

(use-package circe
  :ensure t
  :defer t
  :config
  (progn
    (load-file  "~/.private.el")
    (setq circe-network-options
          `(("Freenode"
             :nick "chunyang"
             :channels ("#emacs")
             :nickserv-password ,freenode-password)))))

(use-package weibo
  :ensure t
  :defer t
  :config
  (progn
    (require 'weibo)
    (setq weibo-consumer-key "3426280940"
          weibo-consumer-secret "9de89c9ef2caf54fc32246885a33bcb4")))

(use-package google-this
  :ensure t
  :diminish google-this-mode
  :config
  (progn
    (global-set-key (kbd "C-c g") #'google-this-mode-submap)
    (global-unset-key (kbd "C-c /"))))

(use-package sx                         ; StackExchange client for Emacs
  :ensure t
  :bind (("C-c w s" . sx-tab-frontpage)
         ("C-c w S" . sx-tab-newest)
         ("C-c w a" . sx-ask)))

(use-package mu4e
  :load-path "lisp/mu4e/"
  :defer t
  :idle (require 'org-mu4e)
  :config
  (progn
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
          mu4e-headers-skip-duplicates t)))


;;; Dictionary
(use-package youdao-dictionary
  :ensure t
  :defer t
  :bind (("C-c y" . youdao-dictionary-search-at-point))
  :config
  (progn
    (setq url-automatic-caching t)
    (push "*Youdao Dictionary*" popwin:special-display-config)))

(use-package osx-dictionary
  :ensure t
  :defer t
  :bind (("C-c d" . osx-dictionary-search-pointer))
  :config
  (push "*osx-dictionary*" popwin:special-display-config))

(use-package helm-dict
  :load-path "personal/"
  ;; TODO: how to add new key to a existing prefix keymap?
  :bind ("M-4" . helm-dict))


;;; Org-mode
(use-package org
  :defer t
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


;;; Online help
(use-package info                       ; Info manual viewer
  :defer t
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
