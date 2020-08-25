;;; init.el --- Emacs init file
;;  Author: Ian Y.E. Pan
;;; Commentary:
;;  This is my personal Emacs configuration
;; Installation: brew install emacs-plus --HEAD --without-spacemacs-icon --with-jansson
;;; Code:
(defvar file-name-handler-alist-original file-name-handler-alist)

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6
      file-name-handler-alist nil
      site-run-file nil)

(defvar ian/gc-cons-threshold (* 100 1024 1024))

(add-hook 'emacs-startup-hook ; hook run after loading init files
          #'(lambda ()
              (setq gc-cons-threshold ian/gc-cons-threshold
                    gc-cons-percentage 0.1
                    file-name-handler-alist file-name-handler-alist-original)))
(add-hook 'minibuffer-setup-hook #'(lambda ()
                                     (setq gc-cons-threshold most-positive-fixnum)))
(add-hook 'minibuffer-exit-hook #'(lambda ()
                                    (garbage-collect)
                                    (setq gc-cons-threshold ian/gc-cons-threshold)))

(require 'package)
(add-to-list 'package-archives '("gnu"   . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("org"   . "https://orgmode.org/elpa/"))
(setq package-enable-at-startup nil)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-and-compile
  (setq use-package-always-ensure t
        use-package-expand-minimally t))

;;; Settings without corresponding packages

(use-package emacs
  :preface
  (defvar ian/indent-width 2)
  :config
  (setq user-full-name "Ian Y.E. Pan")
  (setq frame-title-format '("Emacs"))
  (setq ring-bell-function 'ignore)
  (setq-default default-directory "~/")
  (setq frame-resize-pixelwise t)
  (setq scroll-conservatively 101) ; > 100
  (setq scroll-preserve-screen-position t)
  (setq auto-window-vscroll nil)
  (setq load-prefer-newer t)
  (setq inhibit-compacting-font-caches t)
  (setq echo-keystrokes 0.02)
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (setq-default line-spacing 3)
  (setq-default indent-tabs-mode nil)
  (setq-default tab-width ian/indent-width))

(use-package "startup"
  :ensure nil
  :config
  (setq initial-scratch-message ""))

;; Overriding built-in function
(defun use-fancy-splash-screens-p ()
  "Always display splash screen with Emacs PNG logo."
  t)

;;; Built-in packages

(use-package cus-edit
  :ensure nil
  :config
  (setq custom-file "~/.emacs.d/to-be-dumped.el"))

(use-package scroll-bar
  :ensure nil
  :config
  (scroll-bar-mode -1))

(use-package simple
  :ensure nil
  :config
  (column-number-mode +1))

(use-package "window"
  :ensure nil
  :preface
  (defun ian/split-and-follow-horizontally ()
    "Split window below."
    (interactive)
    (split-window-below)
    (other-window 1))
  (defun ian/split-and-follow-vertically ()
    "Split window right."
    (interactive)
    (split-window-right)
    (other-window 1))
  :config
  (setq split-width-threshold 100)
  (global-set-key (kbd "C-x 2") #'ian/split-and-follow-horizontally)
  (global-set-key (kbd "C-x 3") #'ian/split-and-follow-vertically))

(use-package delsel
  :ensure nil
  :config
  (delete-selection-mode +1))

(use-package files
  :ensure nil
  :config
  (setq confirm-kill-processes nil)
  (setq create-lockfiles nil) ; don't create .# files (crashes 'npm start')
  (setq make-backup-files nil))

(use-package autorevert
  :ensure nil
  :config
  (setq auto-revert-interval 2)
  (setq auto-revert-check-vc-info t)
  (setq global-auto-revert-non-file-buffers t)
  (setq auto-revert-verbose nil)
  (global-auto-revert-mode +1))

(use-package eldoc
  :ensure nil
  :config
  (setq eldoc-idle-delay 0.4))

(use-package js
  :ensure nil
  ;; :mode ("\\.jsx?\\'" . js-jsx-mode)
  :config
  (setq js-indent-level ian/indent-width)
  (add-hook 'flycheck-mode-hook
            #'(lambda ()
                (let* ((root (locate-dominating-file
                              (or (buffer-file-name) default-directory)
                              "node_modules"))
                       (eslint
                        (and root
                             (expand-file-name "node_modules/.bin/eslint"
                                               root))))
                  (when (and eslint (file-executable-p eslint))
                    (setq-local flycheck-javascript-eslint-executable eslint))))))

(use-package cc-vars
  :ensure nil
  :config
  (setq c-default-style '((java-mode . "java")
                          (awk-mode  . "awk")
                          (other     . "k&r")))
  (setq-default c-basic-offset ian/indent-width))

(use-package cc-mode
  :ensure nil
  :config
  (define-key c++-mode-map ":" nil)) ; don't indent namespace:: on-the-fly etc.

(use-package perl-mode
  :ensure nil
  :config
  (setq perl-indent-level ian/indent-width))

(use-package cperl-mode
  :ensure nil
  :config
  (defalias 'perl-mode 'cperl-mode)
  (setq cperl-invalid-face nil)
  (setq cperl-indent-level ian/indent-width))

(use-package prolog
  :ensure nil
  :mode (("\\.pl\\'" . prolog-mode)) ; if commented, ".pl" will become perl/cperl mode
  :config
  (setq prolog-indent-width ian/indent-width))

(use-package python
  :ensure nil
  :config
  (setq python-indent-offset ian/indent-width)
  (setq python-shell-interpreter "python3"))

(use-package css-mode ; inerited by less-css-mode
  :ensure nil
  :config
  (setq css-indent-offset ian/indent-width))

(use-package mwheel
  :ensure nil
  :config
  (setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
  (setq mouse-wheel-progressive-speed nil))

(use-package paren
  :ensure nil
  :init
  (setq show-paren-delay 0)
  :config
  (show-paren-mode +1))

(use-package frame
  :preface
  (defun ian/fontsize-normal ()
    (interactive)
    (set-face-attribute 'default nil :height 160))
  (defun ian/set-default-font ()
    (interactive)
    (when (member "Consolas" (font-family-list))
      (set-face-attribute 'default nil :family "Consolas" :weight 'normal))
    (ian/fontsize-normal))
  (defalias 'ian/normal-fontsize #'ian/fontsize-normal)
  (defalias 'ian/small-fontsize #'ian/fontsize-small)
  :ensure nil
  :config
  (setq default-frame-alist
        (append (list '(width  . 75) '(height . 35)
                      '(internal-border-width . 2))))
  (blink-cursor-mode -1)
  (ian/set-default-font))

(use-package ediff
  :ensure nil
  :config
  (setq ediff-window-setup-function #'ediff-setup-windows-plain)
  (setq ediff-split-window-function #'split-window-horizontally))

(use-package flyspell
  :ensure nil
  :config
  (setq ispell-program-name "/usr/local/bin/aspell"))

(use-package elec-pair
  :ensure nil
  :hook (prog-mode . electric-pair-mode))

(use-package whitespace
  :ensure nil
  :hook (before-save . whitespace-cleanup))

(use-package dired
  :ensure nil
  :hook ((dired-mode . dired-hide-details-mode)
         (dired-mode . hl-line-mode))
  :config
  (setq dired-listing-switches "-lat") ; sort by date (new first)
  (put 'dired-find-alternate-file 'disabled nil))

(use-package saveplace
  :ensure nil
  :config
  (save-place-mode +1))

(use-package recentf
  :ensure nil
  :config
  (add-to-list 'recentf-exclude
               (format "%s/\\.emacs.d/elpa/.*" (getenv "HOME")))
  (recentf-mode +1))

(use-package display-line-numbers
  :ensure nil
  :hook (prog-mode . display-line-numbers-mode)
  :config
  (setq-default display-line-numbers-width 3))

(use-package ox
  :ensure nil
  :config
  (setq org-export-with-smart-quotes t))

(use-package ox-latex
  :ensure nil
  :config
  (setq org-latex-packages-alist '(("margin=1in" "geometry" nil)
                                   ("bitstream-charter" "mathdesign" nil)
                                   ("" "inconsolata" nil)))
  (setq org-latex-pdf-process
        '("/Library/TeX/texbin/pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f; rm *.tex *.out *.aux *.log"
          "/Library/TeX/texbin/pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f; rm *.tex *.out *.aux *.log"
          "/Library/TeX/texbin/pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f; rm *.tex *.out *.aux *.log")))

(use-package faces
  :ensure nil
  :config
  (when (member "Segoe UI" (font-family-list))
    (set-face-attribute 'variable-pitch nil :family "Segoe UI" :weight 'normal :height 1.0)))

;;; Third-party Packages

;; GUI enhancements

(use-package vscode-dark-plus-theme
  :custom-face
  (ivy-current-match ((t (:inherit 'hl-line))))
  :config
  (load-theme 'vscode-dark-plus t))

(use-package highlight-symbol
  :hook (prog-mode . highlight-symbol-mode)
  :config
  (setq highlight-symbol-idle-delay 0.3))

(use-package highlight-numbers
  :hook (prog-mode . highlight-numbers-mode))

(use-package highlight-escape-sequences
  :hook (prog-mode . hes-mode))

(use-package emojify
  :config
  :hook (after-init . global-emojify-mode))

;; Vi keybindings

(use-package evil
  :init
  (setq evil-want-C-u-scroll t
        evil-want-keybinding nil
        evil-shift-width ian/indent-width)
  :hook (after-init . evil-mode)
  :preface
  (defun ian/save-and-kill-this-buffer ()
    (interactive)
    (save-buffer)
    (kill-this-buffer))
  :config
  (setq-default cursor-type  '(hbar . 5))
  (setq evil-emacs-state-cursor '(hbar . 5))
  (with-eval-after-load 'evil-maps
    (define-key evil-normal-state-map (kbd "gd") #'xref-find-definitions)
    (define-key evil-normal-state-map (kbd "<f12>") #'xref-find-definitions)
    (define-key evil-insert-state-map (kbd "C-n") nil) ; avoid conflict with company tooltip selection
    (define-key evil-insert-state-map (kbd "C-p") nil) ; avoid conflict with company tooltip selection
    (define-key evil-normal-state-map (kbd "C-p") nil) ; avoid conflict with WSL find-file
    (define-key evil-insert-state-map (kbd "C-S-C") #'evil-yank)         ; for WSL
    (define-key evil-normal-state-map (kbd "C-S-C") #'evil-yank)         ; for WSL
    (define-key evil-insert-state-map (kbd "C-S-V") #'evil-paste-before) ; for WSL
    )
  (evil-ex-define-cmd "q" #'kill-this-buffer)
  (evil-ex-define-cmd "wq" #'ian/save-and-kill-this-buffer))

(use-package evil-collection
  :after evil
  :config
  (setq evil-collection-company-use-tng nil)
  (evil-collection-init))

(use-package evil-commentary
  :after evil
  :config
  (evil-commentary-mode +1))

(use-package evil-magit)

(use-package evil-matchit
  :hook (web-mode . turn-on-evil-matchit-mode))

;; Git integration

(use-package magit
  :bind ("C-x g" . magit-status)
  :config
  (add-hook 'with-editor-mode-hook #'evil-insert-state))

(use-package diff-hl
  :hook ((prog-mode . diff-hl-mode)
         (diff-hl-mode . diff-hl-flydiff-mode))
  :config
  (setq diff-hl-flydiff-delay 0.05))

;; Searching/sorting enhancements & project management

(use-package ivy
  :hook (after-init . ivy-mode)
  :config
  (setcdr (assoc t ivy-format-functions-alist) #'ivy-format-function-line)
  (setq ivy-height 12)
  (setq ivy-display-style nil)
  (setq ivy-re-builders-alist
        '((counsel-rg            . ivy--regex-plus)
          (counsel-projectile-rg . ivy--regex-plus)
          (swiper                . ivy--regex-plus)
          (t                     . ivy--regex-fuzzy)))
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-initial-inputs-alist nil)
  (define-key ivy-minibuffer-map (kbd "RET") #'ivy-alt-done)
  (define-key ivy-mode-map       (kbd "<escape>") nil)
  (define-key ivy-minibuffer-map (kbd "<escape>") #'minibuffer-keyboard-quit))

(use-package counsel
  :hook (ivy-mode . counsel-mode)
  :config
  (setq counsel-rg-base-command "rg --vimgrep %s")
  (global-set-key (kbd "s-P") #'counsel-M-x)
  (global-set-key (kbd "C-S-p") #'counsel-M-x)
  (global-set-key (kbd "s-f") #'counsel-grep-or-swiper)
  (global-set-key (kbd "C-s") #'counsel-grep-or-swiper))

(use-package counsel-projectile
  :config
  (counsel-projectile-mode +1))

(use-package swiper
  :after ivy
  :config
  (setq swiper-action-recenter t)
  (setq swiper-goto-start-of-match t))

(use-package projectile
  :config
  (setq projectile-sort-order 'recentf)
  (setq projectile-indexing-method 'hybrid)
  (setq projectile-completion-system 'ivy)
  (setq projectile-mode-line-prefix " ")
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "C-c p") #'projectile-command-map)
  (define-key projectile-mode-map (kbd "s-p") #'projectile-find-file)
  (define-key projectile-mode-map (kbd "C-p") #'projectile-find-file)
  (define-key projectile-mode-map (kbd "s-F") #'projectile-ripgrep)
  (define-key projectile-mode-map (kbd "C-S-f") #'projectile-ripgrep))

(use-package wgrep
  :commands wgrep-change-to-wgrep-mode
  :config
  (setq wgrep-auto-save-buffer t))

(use-package prescient
  :config
  (setq prescient-filter-method '(literal regexp initialism fuzzy))
  (prescient-persist-mode +1))

(use-package ivy-prescient
  :after (prescient ivy counsel)
  :config
  (setq ivy-prescient-sort-commands
        '(:not swiper
               counsel-grep
               counsel-rg
               counsel-projectile-rg
               ivy-switch-buffer
               counsel-switch-buffer))
  (setq ivy-prescient-retain-classic-highlighting t)
  (ivy-prescient-mode +1))

(use-package company-prescient
  :after (prescient company)
  :config
  (company-prescient-mode +1))

;; Programming language support and utilities

(use-package lsp-mode
  :hook ((c-mode          ; clangd
          c++-mode        ; clangd
          c-or-c++-mode   ; clangd
          java-mode       ; eclipse-jdtls
          js-mode         ; ts-ls (tsserver wrapper)
          js-jsx-mode     ; ts-ls (tsserver wrapper)
          typescript-mode ; ts-ls (tsserver wrapper)
          python-mode     ; mspyls
          web-mode
          ) . lsp)
  :commands lsp
  :config
  (setq lsp-auto-guess-root t)
  (setq lsp-diagnostic-package :none)             ; disable flycheck-lsp for most modes
  (add-hook 'web-mode-hook #'lsp-flycheck-enable) ; enable flycheck-lsp for web-mode locally
  (setq lsp-enable-symbol-highlighting nil)
  (setq lsp-enable-on-type-formatting nil)
  (setq lsp-signature-auto-activate nil)
  (setq lsp-modeline-code-actions-enable nil)
  (setq lsp-modeline-diagnostics-enable nil)
  (setq lsp-enable-folding nil)
  (setq lsp-enable-imenu nil)
  (setq lsp-enable-snippet nil)
  (setq lsp-enable-completion-at-point nil)
  (setq read-process-output-max (* 1024 1024)) ;; 1mb
  (setq lsp-idle-delay 0.5)
  (setq lsp-prefer-capf t) ; prefer lsp's company-capf over company-lsp
  (add-to-list 'lsp-language-id-configuration '(js-jsx-mode . "javascriptreact")))

(use-package lsp-java
  :after lsp)

(use-package modern-cpp-font-lock
  :hook (c++-mode . modern-c++-font-lock-mode))

(use-package lsp-python-ms
  :hook (python-mode . (lambda () (require 'lsp-python-ms)))
  :config
  (setq lsp-python-ms-executable
        "~/python-language-server/output/bin/Release/osx-x64/publish/Microsoft.Python.LanguageServer")
  (setq lsp-python-ms-python-executable-cmd "python3"))

(use-package pyvenv
  :config
  (setq pyvenv-mode-line-indicator '(pyvenv-virtual-env-name ("[venv:" pyvenv-virtual-env-name "] ")))
  (add-hook 'pyvenv-post-activate-hooks
            #'(lambda ()
                (call-interactively #'lsp-workspace-restart)))
  (pyvenv-mode +1))

(use-package cobol-mode
  :config
  (setq cobol-tab-width ian/indent-width)
  (setq auto-mode-alist
        (append
         '(("\\.cob\\'" . cobol-mode)
           ("\\.cbl\\'" . cobol-mode))
         auto-mode-alist)))

(use-package company
  :hook (prog-mode . company-mode)
  :config
  (setq company-minimum-prefix-length 1)
  (setq company-selection-wrap-around t)
  (setq company-tooltip-align-annotations t)
  (setq company-frontends '(company-pseudo-tooltip-frontend ; show tooltip even for single candidate
                            company-echo-metadata-frontend))
  (with-eval-after-load 'company
    (define-key company-active-map (kbd "C-j") nil) ; avoid conflict with emmet-mode
    (define-key company-active-map (kbd "C-n") #'company-select-next)
    (define-key company-active-map (kbd "C-p") #'company-select-previous)))

(use-package flycheck
  :hook ((prog-mode . flycheck-mode))
  :config
  (setq flycheck-check-syntax-automatically '(save mode-enabled newline))
  (setq flycheck-display-errors-delay 0.1)
  (setq flycheck-flake8rc "~/.config/flake8"))

(use-package org
  :hook ((org-mode . visual-line-mode)
         (org-mode . auto-fill-mode)
         (org-mode . org-indent-mode)
         (org-mode . (lambda () (setq-local evil-auto-indent nil))))
  :config
  (setq org-descriptive-links nil)
  (setq org-startup-folded nil)
  (setq org-todo-keywords '((sequence "TODO" "DOING" "DONE")))
  (setq org-html-checkbox-type 'html))

(use-package org-bullets
  :hook (org-mode . org-bullets-mode))

(use-package markdown-mode)

(use-package web-mode
  :mode (("\\.html?\\'" . web-mode)
         ("\\.css\\'"   . web-mode)
         ("\\.jsx?\\'"  . web-mode)
         ("\\.tsx?\\'"  . web-mode)
         ("\\.json\\'"  . web-mode))
  :config
  (setq web-mode-markup-indent-offset ian/indent-width)
  (setq web-mode-code-indent-offset ian/indent-width)
  (setq web-mode-css-indent-offset ian/indent-width)
  (setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'"))))

(use-package emmet-mode
  :hook ((html-mode       . emmet-mode)
         (css-mode        . emmet-mode)
         (js-mode         . emmet-mode)
         (js-jsx-mode     . emmet-mode)
         (typescript-mode . emmet-mode)
         (web-mode        . emmet-mode))
  :config
  (setq emmet-insert-flash-time 0.001) ; effectively disabling it
  (add-hook 'js-jsx-mode-hook #'(lambda ()
                                  (setq-local emmet-expand-jsx-className? t)))
  (add-hook 'web-mode-hook #'(lambda ()
                               (setq-local emmet-expand-jsx-className? t))))

(use-package cpp-auto-include ; Copyright (C) 2015 by Syohei Yoshida / Ben Deane
  :bind (:map c++-mode-map ("C-c i" . cpp-auto-include/ensure-includes-for-file)))

(use-package format-all
  :preface
  (defun ian/format-code ()
    "Auto-format whole buffer."
    (interactive)
    (if (derived-mode-p 'prolog-mode)
        (prolog-indent-buffer)
      (format-all-buffer)))
  (defalias 'format-document #'ian/format-code))

(use-package rainbow-mode
  :hook (web-mode . rainbow-mode))

(use-package hl-todo
  :config
  (add-to-list 'hl-todo-keyword-faces '("DOING" . "#94bff3"))
  (global-hl-todo-mode +1))

;;; Dired enhancements

(use-package dired-single
  :preface
  (defun ian/dired-single-init ()
    (define-key dired-mode-map [return] #'dired-single-buffer)
    (define-key dired-mode-map [remap dired-mouse-find-file-other-window] #'dired-single-buffer-mouse)
    (define-key dired-mode-map [remap dired-up-directory] #'dired-single-up-directory))
  :config
  (if (boundp 'dired-mode-map)
      (ian/dired-single-init)
    (add-hook 'dired-load-hook #'ian/dired-single-init)))

(use-package dired-subtree
  :ensure t
  :after dired
  :config
  (setq dired-subtree-use-backgrounds nil)
  :bind (:map dired-mode-map ("<tab>" . dired-subtree-toggle)))

;; Terminal integration

(use-package vterm ; when installing, evaluate exec-path first (else 'command not found')
  :hook (vterm-mode . (lambda ()
                        (setq-local global-hl-line-mode nil)
                        (setq-local line-spacing nil)))
  :config
  (define-key vterm-mode-map (kbd "C-l") #'(lambda ()
                                             (interactive)
                                             (vterm-clear)
                                             (vterm-clear-scrollback))))

(use-package vterm-toggle
  :after (projectile vterm evil)
  :config
  (setq vterm-toggle-fullscreen-p nil)
  (setq vterm-toggle-scope 'projectile)
  (with-eval-after-load 'evil
    (evil-set-initial-state 'vterm-mode 'emacs))
  (global-set-key (kbd "C-`") #'vterm-toggle)
  (global-set-key (kbd "s-j") #'vterm-toggle)
  (add-to-list 'display-buffer-alist
               '((lambda (bufname _) (with-current-buffer bufname (equal major-mode 'vterm-mode)))
                 (display-buffer-reuse-window display-buffer-at-bottom)
                 (reusable-frames . visible)
                 (window-height . 0.7))))

;; Misc

(use-package pdf-tools
  :mode (("\\.pdf\\'" . pdf-view-mode))
  :bind ((:map pdf-view-mode-map ("C--" . pdf-view-shrink))
         (:map pdf-view-mode-map ("C-=" . pdf-view-enlarge))
         (:map pdf-view-mode-map ("C-0" . pdf-view-scale-reset)))
  :config
  (pdf-loader-install))

(use-package smart-mode-line
  :config
  (setq sml/no-confirm-load-theme t)
  (setq sml/modified-char "*")
  (sml/setup))

(use-package minions
  :config
  (setq minions-mode-line-lighter "")
  (setq minions-mode-line-delimiters '("" . ""))
  (minions-mode +1))

(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(provide 'init)
;;; init.el ends here
