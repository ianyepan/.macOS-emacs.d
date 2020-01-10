;;; init.el --- Emacs init file
;;  Author: Ian Y.E. Pan
;;; Commentary:
;;  This is my personal Emacs configuration
;;; Code:
(defvar file-name-handler-alist-original file-name-handler-alist)

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6
      file-name-handler-alist nil
      site-run-file nil)

(defvar ian/gc-cons-threshold 20000000)

(add-hook 'emacs-startup-hook ; hook run after loading init files
          #'(lambda ()
              (setq gc-cons-threshold ian/gc-cons-threshold
                    gc-cons-percentage 0.1
                    file-name-handler-alist file-name-handler-alist-original)))

(add-hook 'minibuffer-setup-hook #'(lambda ()
                                     (setq gc-cons-threshold (* ian/gc-cons-threshold 2))))
(add-hook 'minibuffer-exit-hook #'(lambda ()
                                    (garbage-collect)
                                    (setq gc-cons-threshold ian/gc-cons-threshold)))

(require 'package)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/"))
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
  (custom-file "~/.config/emacs/to-be-dumped.el"))

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
  (global-set-key (kbd "C-x 2") #'ian/split-and-follow-horizontally)
  (global-set-key (kbd "C-x 3") #'ian/split-and-follow-vertically))

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
  :diminish
  :hook (prog-mode . eldoc-mode)
  :custom
  (eldoc-idle-delay 0.4))

(use-package js
  :ensure nil
  :mode ("\\.jsx?\\'" . js-mode)
  :custom
  (js-indent-level ian/indent-width))

(use-package xref
  :ensure nil
  :config
  (define-key prog-mode-map (kbd "s-b") #'xref-find-definitions)
  (define-key prog-mode-map (kbd "s-[") #'xref-pop-marker-stack))

(use-package cc-vars
  :ensure nil
  :custom
  (c-default-style '((java-mode . "java")
                     (awk-mode . "awk")
                     (other . "k&r")))
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
  (initial-frame-alist (quote ((fullscreen . maximized))))
  :config
  (blink-cursor-mode -1)
  (when (member "Source Code Pro" (font-family-list))
    (set-frame-font "Source Code Pro-12:weight=regular" t t)))

(use-package ediff
  :ensure nil
  :custom
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
                (set-face-attribute face nil :weight 'normal))) (face-list)))
  :config
  (add-hook 'after-init-hook #'ian/disable-bold-and-fringe-bg-face-globally))

(use-package flyspell
  :ensure nil
  :diminish
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
  (delete-by-moving-to-trash t))

(use-package saveplace
  :config
  (save-place-mode +1))

(use-package recentf
  :config
  (recentf-mode +1))

;;; Third-party Packages

;; GUI enhancements

(add-to-list 'custom-theme-load-path "~/.config/emacs/themes/")
(load-theme 'vscode-default-dark t)

(use-package solaire-mode
  :hook (((change-major-mode after-revert ediff-prepare-buffer) . turn-on-solaire-mode)
         (minibuffer-setup . solaire-mode-in-minibuffer))
  :config
  (solaire-global-mode)
  (solaire-mode-swap-bg))

(use-package dashboard
  :hook (dashboard-mode . (lambda () (setq default-directory "~/")))
  :custom
  (dashboard-startup-banner 'logo)
  (dashboard-banner-logo-title "Dangerously powerful")
  (dashboard-items nil)
  (dashboard-set-footer nil)
  :config
  (dashboard-setup-startup-hook))

(use-package smart-mode-line
  :custom
  (sml/no-confirm-load-theme t)
  :config
  (when (member "Menlo" (font-family-list))
    (set-face-attribute 'mode-line nil :height 110 :font "Menlo")
    (set-face-attribute 'mode-line-inactive nil :height 110 :font "Menlo"))
  (sml/setup))

(use-package all-the-icons
  :custom
  (all-the-icons-scale-factor 1.0))

(use-package all-the-icons-ivy
  :hook (after-init . all-the-icons-ivy-setup))

(use-package centaur-tabs
  :demand
  :init
  (setq centaur-tabs-set-bar 'over)
  :bind (("C-S-<tab>" . centaur-tabs-backward)
         ("C-<tab>" . centaur-tabs-forward))
  :custom
  (centaur-tabs-set-modified-marker t)
  (centaur-tabs-modified-marker " ● ")
  (centaur-tabs-cycle-scope 'tabs)
  (centaur-tabs-height 30)
  (centaur-tabs-set-icons t)
  (centaur-tabs-close-button " × ")
  :config
  (centaur-tabs-mode +1)
  (centaur-tabs-headline-match)
  (centaur-tabs-group-by-projectile-project)
  (when (member "Arial" (font-family-list))
    (centaur-tabs-change-fonts "Arial" 120)))

(use-package highlight-indent-guides
  :diminish
  :hook (prog-mode . highlight-indent-guides-mode)
  :custom
  (highlight-indent-guides-method 'character)
  (highlight-indent-guides-character 9615) ; left-align vertical bar
  (highlight-indent-guides-auto-character-face-perc 20))

(use-package highlight-symbol
  :diminish
  :hook (prog-mode . highlight-symbol-mode)
  :custom-face (highlight-symbol-face ((t (:background "#383439")))) ; twilight
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
  :diminish undo-tree-mode
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
    (define-key evil-insert-state-map (kbd "C-p") nil))
  (evil-ex-define-cmd "q" #'kill-this-buffer)
  (evil-ex-define-cmd "wq" #'ian/save-and-kill-this-buffer))

(use-package evil-collection
  :after evil
  :config
  (setq evil-collection-company-use-tng nil)
  (evil-collection-init))

(use-package evil-commentary
  :after evil
  :diminish
  :config
  (evil-commentary-mode +1))

(use-package evil-magit)

;; Git integration

(use-package magit
  :bind ("C-x g" . magit-status)
  :config
  (add-hook 'with-editor-mode-hook #'evil-insert-state))

(use-package diff-hl
  :config
  (global-diff-hl-mode +1)
  (diff-hl-flydiff-mode +1)
  (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh t))

;; Searching/sorting enhancements & project management

(use-package flx)

(use-package counsel
  :diminish
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
  :diminish
  :hook (after-init . ivy-mode)
  :custom
  (ivy-display-style nil)
  (ivy-re-builders-alist '((counsel-rg . ivy--regex-plus)
                           (counsel-projectile-rg . ivy--regex-plus)
                           (counsel-ag . ivy--regex-plus)
                           (counsel-projectile-ag . ivy--regex-plus)
                           (swiper . ivy--regex-plus)
                           (t . ivy--regex-fuzzy)))
  (ivy-use-virtual-buffers t)
  (ivy-count-format "(%d/%d) ")
  (ivy-initial-inputs-alist nil)
  :config
  (define-key ivy-minibuffer-map (kbd "RET") #'ivy-alt-done)
  (define-key ivy-minibuffer-map (kbd "<escape>") #'minibuffer-keyboard-quit))

(use-package swiper
  :after ivy
  ;; :custom-face (swiper-line-face ((t (:foreground "#ffffff" :background "#60648E"))))
  :custom
  (swiper-action-recenter t)
  (swiper-goto-start-of-match t))

(use-package ivy-posframe
  :after ivy
  :diminish
  :custom
  (ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-top-center)))
  (ivy-posframe-height-alist '((t . 20)))
  (ivy-posframe-parameters '((internal-border-width . 10)))
  (ivy-posframe-width 70)
  :config
  (ivy-posframe-mode +1))

(use-package ivy-rich
  :preface
  (defun ivy-rich-switch-buffer-icon (candidate)
    (with-current-buffer
        (get-buffer candidate)
      (all-the-icons-icon-for-mode major-mode)))
  :init
  (setq ivy-rich-display-transformers-list ; max column width sum = (ivy-poframe-width - 1)
        '(ivy-switch-buffer
          (:columns
           ((ivy-rich-switch-buffer-icon (:width 2))
            (ivy-rich-candidate (:width 35))
            (ivy-rich-switch-buffer-project (:width 15 :face success))
            (ivy-rich-switch-buffer-major-mode (:width 13 :face warning)))
           :predicate
           #'(lambda (cand) (get-buffer cand)))
          counsel-M-x
          (:columns
           ((counsel-M-x-transformer (:width 35))
            (ivy-rich-counsel-function-docstring (:width 34 :face font-lock-doc-face))))
          counsel-describe-function
          (:columns
           ((counsel-describe-function-transformer (:width 35))
            (ivy-rich-counsel-function-docstring (:width 34 :face font-lock-doc-face))))
          counsel-describe-variable
          (:columns
           ((counsel-describe-variable-transformer (:width 35))
            (ivy-rich-counsel-variable-docstring (:width 34 :face font-lock-doc-face))))
          package-install
          (:columns
           ((ivy-rich-candidate (:width 25))
            (ivy-rich-package-version (:width 12 :face font-lock-comment-face))
            (ivy-rich-package-archive-summary (:width 7 :face font-lock-builtin-face))
            (ivy-rich-package-install-summary (:width 23 :face font-lock-doc-face))))))
  :config
  (ivy-rich-mode +1)
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line))

(use-package projectile
  :diminish
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
  (ivy-prescient-sort-commands '(:not swiper counsel-grep ivy-switch-buffer))
  (ivy-prescient-retain-classic-highlighting t)
  :config
  (ivy-prescient-mode +1))

(use-package company-prescient
  :after (prescient company)
  :config
  (company-prescient-mode +1))

;; Programming language support and utilities

(use-package lsp-mode
  :hook ((c-mode         ; clangd
          c-or-c++-mode  ; clangd
          java-mode      ; eclipse-jdtls
          js-mode        ; typescript-language-server
          python-mode    ; mspyls
          ) . lsp)
  :commands lsp
  :custom
  (lsp-prefer-flymake nil)
  (lsp-enable-symbol-highlighting nil)
  (lsp-signature-auto-activate nil))

(use-package lsp-java
  :after lsp)

(use-package lsp-python-ms
  :hook (python-mode . (lambda () (require 'lsp-python-ms)))
  :custom
  (lsp-python-ms-executable
   "~/python-language-server/output/bin/Release/osx-x64/publish/Microsoft.Python.LanguageServer"))

(use-package pyvenv
  :diminish
  :custom
  (pyvenv-mode-line-indicator '(pyvenv-virtual-env-name ("[venv:" pyvenv-virtual-env-name "] ")))
  :config
  (pyvenv-mode +1))

(use-package company-lsp
  :commands company-lsp
  :custom
  (company-lsp-cache-candidates 'auto)
  :config
  (add-to-list 'company-lsp-filter-candidates '(mspyls . t))
  (defun company-lsp--on-completion (response prefix)
    "Handle completion RESPONSE.
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
  :diminish
  :hook (prog-mode . company-mode)
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.1)
  (company-selection-wrap-around t)
  (company-tooltip-align-annotations t)
  (company-frontends '(company-pseudo-tooltip-frontend ; show tooltip even for single candidate
                       company-echo-metadata-frontend))
  :config
  (with-eval-after-load 'company
    (define-key company-active-map (kbd "C-n") #'company-select-next)
    (define-key company-active-map (kbd "C-p") #'company-select-previous)))

(use-package flycheck
  :hook (prog-mode . flycheck-mode)
  :custom
  (flycheck-check-syntax-automatically '(save mode-enabled newline))
  (flycheck-python-flake8-executable "python3")
  (flycheck-flake8rc "~/.config/flake8")
  :config
  (setq-default flycheck-disabled-checkers '(python-pylint)))

(use-package org
  :hook ((org-mode . visual-line-mode)
         (org-mode . org-indent-mode))
  :config
  (with-eval-after-load 'org
    (define-key org-mode-map (kbd "C-<tab>") nil)))

(use-package org-bullets
  :hook (org-mode . org-bullets-mode))

(use-package markdown-mode
  :hook (markdown-mode . visual-line-mode))

(use-package yasnippet
  :diminish yas-minor-mode
  :preface
  (defvar tmp/company-point nil)
  :config
  (yas-global-mode +1)
  (advice-add 'company-complete-common
              :before
              #'(lambda ()
                  (setq tmp/company-point (point))))
  (advice-add 'company-complete-common
              :after
              #'(lambda ()
                  (when (equal tmp/company-point (point))
                    (yas-expand)))))

(use-package yasnippet-snippets)

(use-package json-mode)

(use-package web-mode
  :mode (("\\.tsx?\\'" . web-mode)
         ("\\.css\\'" . web-mode)
         ("\\.html?\\'" . web-mode))
  :custom
  (web-mode-markup-indent-offset ian/indent-width)
  (web-mode-code-indent-offset ian/indent-width)
  (web-mode-css-indent-offset ian/indent-width))

(use-package emmet-mode
  :diminish
  :hook ((html-mode . emmet-mode)
         (css-mode . emmet-mode)
         (js-mode . emmet-mode)
         (web-mode . emmet-mode))
  :config (setq emmet-expand-jsx-className? t))

(use-package format-all
  :preface
  (defun ian/format-code ()
    "Auto-format whole buffer."
    (interactive)
    (if (derived-mode-p 'prolog-mode)
        (prolog-indent-buffer)
      (format-all-buffer)))
  (defun format-document()
    "Auto-format whole buffer (VSCode syntax)."
    (interactive)
    (ian/format-code)))

;; Terminal emulation

(use-package vterm ; when installing, evaluate exec-path first (else 'command not found')
  :custom-face ; twilight
  (vterm-color-default ((t (:foreground "#CCCCCC"))))
  (vterm-color-black   ((t (:foreground "#000000"))))
  (vterm-color-red     ((t (:foreground "#C06D44"))))
  (vterm-color-green   ((t (:foreground "#A6C176"))))
  (vterm-color-yellow  ((t (:foreground "#CDA869"))))
  (vterm-color-blue    ((t (:foreground "#7587A6"))))
  (vterm-color-magenta ((t (:foreground "#B4BE7C"))))
  (vterm-color-cyan    ((t (:foreground "#7F9F98"))))
  (vterm-color-white   ((t (:foreground "#E0E0DF")))))

(use-package vterm-toggle
  :after evil
  :custom
  (vterm-toggle-fullscreen-p nil)
  :config
  (evil-set-initial-state 'vterm-mode 'emacs)
  (global-set-key (kbd "C-`") #'vterm-toggle)
  (global-set-key (kbd "s-j") #'vterm-toggle)
  (add-to-list 'display-buffer-alist
               '("^v?term.*"
                 (display-buffer-reuse-window display-buffer-at-bottom)
                 (reusable-frames . visible)
                 (window-height . 0.5))))

;; Miscellaneous

(use-package diminish
  :demand t)

(use-package which-key
  :diminish
  :custom
  (which-key-idle-delay 0.4)
  (which-key-idle-secondary-delay 0.4)
  :config
  (which-key-mode +1))

(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package rainbow-mode
  :diminish
  :hook (prog-mode . rainbow-mode))

(use-package rainbow-delimiters
  :diminish
  :hook (prog-mode . rainbow-delimiters-mode))

(provide 'init)
;;; init.el ends here
