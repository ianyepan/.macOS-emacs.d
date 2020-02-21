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

(defvar ian/gc-cons-threshold 100000000)

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
  :custom
  (user-full-name "Ian Y.E. Pan")
  (frame-title-format '("Emacs"))
  (ring-bell-function 'ignore)
  (default-directory "~/")
  (frame-resize-pixelwise t)
  (scroll-conservatively 10000)
  (scroll-preserve-screen-position t)
  (auto-window-vscroll nil)
  (load-prefer-newer t)
  :config
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (setq-default line-spacing 3
                indent-tabs-mode nil
                tab-width ian/indent-width))

;;; Built-in packages

(use-package "startup"
  :ensure nil
  :custom
  (inhibit-startup-screen t))

(use-package cus-edit
  :ensure nil
  :custom
  (custom-file "~/.emacs.d/to-be-dumped.el"))

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
  :config
  (global-set-key (kbd "C-x 2") #'(lambda ()
                                    (interactive)
                                    (split-window-below)
                                    (other-window 1)))
  (global-set-key (kbd "C-x 3") #'(lambda ()
                                    (interactive)
                                    (split-window-right)
                                    (other-window 1))))

(use-package delsel
  :ensure nil
  :config
  (delete-selection-mode +1))

(use-package files
  :ensure nil
  :custom
  (confirm-kill-processes nil)
  (make-backup-files nil))

(use-package autorevert
  :ensure nil
  :custom
  (auto-revert-interval 2)
  (auto-revert-check-vc-info t)
  (global-auto-revert-non-file-buffers t)
  (auto-revert-verbose nil)
  :config
  (global-auto-revert-mode +1))

(use-package eldoc
  :ensure nil
  :custom
  (eldoc-idle-delay 0.4))

(use-package js
  :ensure nil
  :mode ("\\.jsx?\\'" . js-mode)
  :custom
  (js-indent-level ian/indent-width)
  :config
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
  :custom
  (c-default-style '((java-mode . "java")
                     (awk-mode  . "awk")
                     (other     . "k&r")))
  :config
  (setq-default c-basic-offset ian/indent-width))

(use-package prolog
  :ensure nil
  :mode (("\\.pl\\'" . prolog-mode))
  :custom
  (prolog-indent-width ian/indent-width))

(use-package python
  :ensure nil
  :custom
  (python-indent-offset ian/indent-width)
  (python-shell-interpreter "python3"))

(use-package mwheel
  :ensure nil
  :custom
  (mouse-wheel-scroll-amount '(1 ((shift) . 1)))
  (mouse-wheel-progressive-speed nil))

(use-package paren
  :ensure nil
  :init
  (setq show-paren-delay 0)
  :config
  (show-paren-mode +1))

(use-package frame
  :ensure nil
  :custom
  (initial-frame-alist '((fullscreen . maximized)))
  :config
  (blink-cursor-mode -1)
  (when (member "Source Code Pro" (font-family-list))
    (set-frame-font "Source Code Pro-14:weight=regular" t t)))

(use-package ediff
  :ensure nil
  :custom
  (ediff-window-setup-function #'ediff-setup-windows-plain)
  (ediff-split-window-function #'split-window-horizontally))

(use-package faces
  :ensure nil
  :preface
  (defun ian/disable-bold-and-fringe-bg-face-globally ()
    "Disable bold face and fringe background in Emacs."
    (interactive)
    (set-face-attribute 'fringe nil :background nil)
    (mapc #'(lambda (face)
              (when (eq (face-attribute face :weight) 'bold)
                (set-face-attribute face nil :weight 'normal)))
          (face-list)))
  :config
  (add-hook 'after-init-hook #'ian/disable-bold-and-fringe-bg-face-globally))

(use-package flyspell
  :ensure nil
  :custom
  (ispell-program-name "/usr/local/bin/aspell"))

(use-package elec-pair
  :ensure nil
  :hook (prog-mode . electric-pair-mode))

(use-package whitespace
  :ensure nil
  :hook (before-save . whitespace-cleanup))

(use-package dired
  :ensure nil
  :custom
  (delete-by-moving-to-trash t)
  :config
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

;; (use-package erc
;;   :hook (erc-mode . erc-scrolltobottom-mode)
;;   :custom
;;   (erc-autojoin-channels-alist '(("freenode.net" "#emacs")
;;                                  ("freenode.net" "#vim")
;;                                  ("freenode.net" "#cyberpunk")))
;;   (erc-interpret-mirc-color t)
;;   (erc-nick "bobbyquine_")
;;   (erc-insert-timestamp-function #'erc-insert-timestamp-left)
;;   (erc-timestamp-format "%H:%M:%S | ")
;;   (erc-track-enable-keybindings t)
;;   :config
;;   (global-set-key (kbd "C-c C-b") #'erc-switch-to-buffer)
;;   (with-eval-after-load 'evil
;;     (evil-set-initial-state 'erc-mode 'emacs)))

;;; Third-party Packages

;; GUI enhancements

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'default-dark t)

;; (use-package doom-themes
;;   :custom-face
;;   (highlight-symbol-face ((t (:background "#404040"))))
;;   :custom
;;   (doom-themes-enable-bold nil)
;;   :config
;;   (load-theme 'doom-outrun-electric t))

(use-package highlight-symbol
  :hook (prog-mode . highlight-symbol-mode)
  :custom
  (highlight-symbol-idle-delay 0.3))

(use-package highlight-numbers
  :hook (prog-mode . highlight-numbers-mode))

(use-package highlight-operators
  :hook (prog-mode . highlight-operators-mode))

(use-package highlight-escape-sequences
  :hook (prog-mode . hes-mode))

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
  (with-eval-after-load 'evil-maps ; avoid conflict with company tooltip selection
    (define-key evil-insert-state-map (kbd "C-n") nil)
    (define-key evil-insert-state-map (kbd "C-p") nil)
    (define-key evil-normal-state-map (kbd "gd") #'xref-find-definitions))
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

;; Git integration

(use-package magit
  :bind ("C-x g" . magit-status)
  :config
  (add-hook 'with-editor-mode-hook #'evil-insert-state))

(use-package git-gutter
  :custom
  (git-gutter:update-interval 0.05)
  :config
  (global-git-gutter-mode +1))

(use-package git-gutter-fringe
  :config
  (setq-default fringes-outside-margins t)
  (define-fringe-bitmap 'git-gutter-fr:added [224]
    nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224]
    nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240]
    nil nil 'bottom))

;; Searching/sorting enhancements & project management

(use-package counsel
  :hook (ivy-mode . counsel-mode)
  :custom
  (counsel-rg-base-command "rg --vimgrep %s")
  :config
  (global-set-key (kbd "s-P") #'counsel-M-x)
  (global-set-key (kbd "s-f") #'counsel-grep-or-swiper))

(use-package counsel-projectile
  :config
  (counsel-projectile-mode +1))

(use-package ivy
  :hook (after-init . ivy-mode)
  :custom
  (ivy-height 15)
  (ivy-display-style nil)
  (ivy-re-builders-alist '((counsel-rg            . ivy--regex-plus)
                           (counsel-projectile-rg . ivy--regex-plus)
                           (counsel-ag            . ivy--regex-plus)
                           (counsel-projectile-ag . ivy--regex-plus)
                           (swiper                . ivy--regex-plus)
                           (t                     . ivy--regex-fuzzy)))
  (ivy-use-virtual-buffers t)
  (ivy-count-format "(%d/%d) ")
  (ivy-initial-inputs-alist nil)
  :config
  (define-key ivy-minibuffer-map (kbd "RET") #'ivy-alt-done)
  (define-key ivy-minibuffer-map (kbd "<escape>") #'minibuffer-keyboard-quit))

(use-package swiper
  :after ivy
  :custom
  (swiper-action-recenter t)
  (swiper-goto-start-of-match t))

(use-package ivy-rich
  :config
  (ivy-rich-mode +1)
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line))

(use-package projectile
  :custom
  (projectile-sort-order 'recentf)
  (projectile-indexing-method 'hybrid)
  (projectile-completion-system 'ivy)
  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "C-c p") #'projectile-command-map)
  (define-key projectile-mode-map (kbd "s-p") #'projectile-find-file)
  (define-key projectile-mode-map (kbd "s-F") #'projectile-ripgrep))

(use-package wgrep
  :commands wgrep-change-to-wgrep-mode
  :custom
  (wgrep-auto-save-buffer t))

(use-package prescient
  :custom
  (prescient-filter-method '(literal regexp initialism fuzzy))
  :config
  (prescient-persist-mode +1))

(use-package ivy-prescient
  :after (prescient ivy)
  :custom
  (ivy-prescient-sort-commands
   '(:not swiper
          counsel-grep
          counsel-rg
          counsel-projectile-rg
          ivy-switch-buffer
          counsel-switch-buffer))
  (ivy-prescient-retain-classic-highlighting t)
  :config
  (ivy-prescient-mode +1))

(use-package company-prescient
  :after (prescient company)
  :config
  (company-prescient-mode +1))

;; Programming language support and utilities

(use-package lsp-mode
  :hook ((c-mode          ; clangd
          c-or-c++-mode   ; clangd
          java-mode       ; eclipse-jdtls
          js-mode         ; ts-ls (tsserver wrapper)
          typescript-mode ; ts-ls (tsserver wrapper)
          python-mode     ; mspyls
          ) . lsp)
  :commands lsp
  :custom
  (lsp-diagnostic-package :none)
  (lsp-enable-symbol-highlighting nil)
  (lsp-enable-on-type-formatting nil)
  (lsp-signature-auto-activate nil)
  (read-process-output-max (* 1024 1024)) ;; 1mb
  (lsp-idle-delay 0.5)
  (lsp-prefer-capf t))

(use-package lsp-java
  :after lsp)

(use-package lsp-python-ms
  :hook (python-mode . (lambda () (require 'lsp-python-ms)))
  :custom
  (lsp-python-ms-executable
   "~/python-language-server/output/bin/Release/osx-x64/publish/Microsoft.Python.LanguageServer")
  (lsp-python-ms-python-executable-cmd "python3"))

(use-package pyvenv
  :custom
  (pyvenv-mode-line-indicator '(pyvenv-virtual-env-name ("[venv:" pyvenv-virtual-env-name "] ")))
  :config
  (add-hook 'pyvenv-post-activate-hooks
            #'(lambda ()
                (call-interactively #'lsp-workspace-restart)))
  (pyvenv-mode +1))

(use-package typescript-mode) ; for looking up JS function definitions

(use-package cobol-mode
  :custom
  (cobol-tab-width 2)
  :config
  (setq auto-mode-alist
        (append
         '(("\\.cob\\'" . cobol-mode)
           ("\\.cbl\\'" . cobol-mode))
         auto-mode-alist)))

(use-package company-lsp
  :commands company-lsp
  :custom
  (company-lsp-cache-candidates 'auto)
  :config
  (push 'company-lsp company-backends)
  (add-to-list 'company-lsp-filter-candidates '(mspyls . t))
  (defun company-lsp--on-completion (response prefix)
    "Note: This is a (hack) workaround for candidate filtering issues in mspyls.
Handle completion RESPONSE.
PREFIX is a string of the prefix when the completion is requested.
Return a list of strings as the completion candidates."
    (let* ((incomplete (and (hash-table-p response) (gethash "isIncomplete" response)))
           (items (cond ((hash-table-p response) (gethash "items" response))
                        ((sequencep response) response)))
           (candidates (mapcar (lambda (item)
                                 (company-lsp--make-candidate item prefix))
                               (lsp--sort-completions items)))
           (server-id (lsp--client-server-id (lsp--workspace-client lsp--cur-workspace)))
           (should-filter (or (eq company-lsp-cache-candidates 'auto) ; change from t to 'auto
                              (and (null company-lsp-cache-candidates)
                                   (company-lsp--get-config company-lsp-filter-candidates server-id)))))
      (when (null company-lsp--completion-cache)
        (add-hook 'company-completion-cancelled-hook #'company-lsp--cleanup-cache nil t)
        (add-hook 'company-completion-finished-hook #'company-lsp--cleanup-cache nil t))
      (when (eq company-lsp-cache-candidates 'auto)
        ;; Only cache candidates on auto mode. If it's t company caches the
        ;; candidates for us.
        (company-lsp--cache-put prefix (company-lsp--cache-item-new candidates incomplete)))
      (if should-filter
          (company-lsp--filter-candidates candidates prefix)
        candidates))))

(use-package company
  :hook (prog-mode . company-mode)
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0)
  (company-selection-wrap-around t)
  (company-tooltip-align-annotations t)
  (company-frontends '(company-pseudo-tooltip-frontend ; show tooltip even for single candidate
                       company-echo-metadata-frontend))
  :config
  (with-eval-after-load 'company
    (define-key company-active-map (kbd "C-j") nil) ; avoid conflict with emmet-mode
    (define-key company-active-map (kbd "C-n") #'company-select-next)
    (define-key company-active-map (kbd "C-p") #'company-select-previous)))

(use-package company-posframe
  :custom
  (company-posframe-show-metadata nil)
  (company-posframe-show-indicator nil)
  (company-posframe-quickhelp-delay nil)
  :config
  (company-posframe-mode +1))

(use-package flycheck
  :hook ((prog-mode   . flycheck-mode))
  :custom
  (flycheck-check-syntax-automatically '(save mode-enabled newline))
  (flycheck-display-errors-delay 0.1)
  (flycheck-flake8rc "~/.config/flake8"))

(use-package flycheck-posframe
  :after flycheck
  :hook (flycheck-mode . flycheck-posframe-mode)
  :custom
  (flycheck-posframe-position 'window-bottom-left-corner)
  (flycheck-posframe-warning-prefix "\u26a0 ")
  (flycheck-posframe-error-prefix "\u26a0 "))

(use-package org
  :hook ((org-mode . visual-line-mode)
         (org-mode . org-indent-mode))
  :config
  (with-eval-after-load 'org
    (define-key org-mode-map (kbd "C-<tab>") nil)))

(use-package org-bullets
  :hook (org-mode . org-bullets-mode))

(use-package markdown-mode)

(use-package web-mode
  :mode (("\\.html?\\'" . web-mode)
         ("\\.css\\'"   . web-mode)
         ("\\.json\\'"  . web-mode))
  :custom
  (web-mode-markup-indent-offset ian/indent-width)
  (web-mode-code-indent-offset ian/indent-width)
  (web-mode-css-indent-offset ian/indent-width))

(use-package emmet-mode
  :hook ((html-mode . emmet-mode)
         (css-mode  . emmet-mode)
         (js-mode   . emmet-mode)
         (web-mode  . emmet-mode))
  :custom
  (emmet-insert-flash-time 0.001) ; basically disabling it
  :config
  (add-hook 'js-mode-hook #'(lambda ()
                              (setq-local emmet-expand-jsx-className? t))))

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
  (global-hl-todo-mode +1))

;; Terminal emulation

(use-package vterm ; when installing, evaluate exec-path first (else 'command not found')
  :hook (vterm-mode . (lambda ()
                        (setq-local global-hl-line-mode nil)
                        (setq-local line-spacing nil))))

(use-package vterm-toggle
  :after evil
  :custom
  (vterm-toggle-fullscreen-p nil)
  :config
  (with-eval-after-load 'evil
    (evil-set-initial-state 'vterm-mode 'emacs))
  (global-set-key (kbd "C-`") #'vterm-toggle)
  (global-set-key (kbd "s-j") #'vterm-toggle)
  (add-to-list 'display-buffer-alist
               '("^v?term.*"
                 (display-buffer-reuse-window display-buffer-at-bottom)
                 (reusable-frames . visible)
                 (window-height . 0.5))))

;; IRC relay remote client

(use-package weechat
  :hook (weechat-mode . (lambda () (visual-line-mode +1)))
  :custom
  (weechat-port-default 1234)
  (weechat-fill-column 80)
  (weechat-notification-mode t)
  (weechat-buffer-kill-buffers-on-disconnect t)
  :config
  (add-hook 'weechat-connect-hook #'weechat-monitor-all-buffers)
  (global-set-key (kbd "C-c C-b") #'weechat-switch-buffer)
  (add-to-list 'weechat-modules 'weechat-notifications)
  (add-to-list 'weechat-modules 'weechat-tracking)
  (with-eval-after-load 'evil
    (evil-set-initial-state 'weechat-mode 'emacs)))

;; Miscellaneous

(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

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

(use-package neotree
  :preface
  (defun neotree-project-toggle ()
    "Open NeoTree using the git root."
    (interactive)
    (let ((project-dir (projectile-project-root))
          (file-name (buffer-file-name)))
      (neotree-toggle)
      (if project-dir
          (if (neo-global--window-exists-p)
              (progn
                (neotree-dir project-dir)
                (neotree-find file-name)))
        (message "Could not find git project root."))))
  :init
  (global-set-key (kbd "s-b") #'neotree-project-toggle)
  :hook (neotree-mode . hl-line-mode)
  :custom
  (neo-theme 'nerd)
  (neo-show-hidden-files t)
  (neo-window-width 30))

(use-package diminish
  :preface
  (defun diminish-most-modes (unused)
    "Hide most minor modes except the ones listed here."
    (interactive)
    (dolist (mode
             (seq-filter
              (lambda (elt)
                (not (seq-contains '(projectile-mode
                                     lsp-mode
                                     flycheck-mode)
                                   elt)))
              minor-mode-list))
      (diminish mode)))
  :config
  (add-to-list 'after-load-functions #'diminish-most-modes))

(provide 'init)
;;; init.el ends here
