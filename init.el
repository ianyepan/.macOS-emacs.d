;;; init.el --- Emacs init file
;;  Author: Ian Y.E. Pan
;;; Commentary:
;;  This is my personal Emacs configuration
;;; Code:
(defvar file-name-handler-alist-original file-name-handler-alist)

(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6
      file-name-handler-alist nil
      site-run-file nil)

(add-hook 'emacs-startup-hook ; hook run after loading init files
          (lambda ()
            (setq gc-cons-threshold 20000000
                  gc-cons-percentage 0.1
                  file-name-handler-alist file-name-handler-alist-original)))

(add-hook 'minibuffer-setup-hook (lambda () (setq gc-cons-threshold 40000000)))
(add-hook 'minibuffer-exit-hook (lambda ()
                                  (garbage-collect)
                                  (setq gc-cons-threshold 20000000)))

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(setq package-enable-at-startup nil)
(package-initialize)

(setq custom-file "~/.emacs.d/to-be-dumped.el") ; custom generated, don't load

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-and-compile
  (setq use-package-always-ensure t))

(setq user-full-name "Ian Y.E. Pan"
      frame-title-format '("Emacs")
      ring-bell-function 'ignore
      default-directory "~/"
      mouse-highlight nil)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(column-number-mode +1)
(setq frame-resize-pixelwise t)
(setq scroll-margin 0
      scroll-conservatively 10000
      scroll-preserve-screen-position t
      auto-window-vscroll nil)
(setq-default line-spacing 3
              indent-tabs-mode nil
              tab-width 2)

(defun ian/load-init()
  "Reload `.emacs.d/init.el'."
  (interactive)
  (load-file "~/.emacs.d/init.el"))

(defun ian/hide-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

(defun ian/align-whitespace (start end)
  "Align columns by whitespace."
  (interactive "r")
  (align-regexp start end
                "\\(\\s-*\\)\\s-" 1 0 t))

;;; Built-in packages

(use-package "startup"
  :ensure nil
  :config (setq inhibit-startup-screen t))

(use-package "window"
  :ensure nil
  :config
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
  (global-set-key (kbd "C-x 2") 'ian/split-and-follow-horizontally)
  (global-set-key (kbd "C-x 3") 'ian/split-and-follow-vertically))

(use-package delsel
  :ensure nil
  :config (delete-selection-mode +1))

(use-package files
  :ensure nil
  :config
  (setq confirm-kill-processes nil
        make-backup-files nil))

(use-package autorevert
  :ensure nil
  :hook (after-init . global-auto-revert-mode)
  :config
  (setq auto-revert-interval 2
        auto-revert-check-vc-info t
        auto-revert-verbose nil))

(use-package eldoc
  :ensure nil
  :diminish eldoc-mode
  :config
  (global-eldoc-mode -1)
  (add-hook 'prog-mode-hook 'eldoc-mode)
  (setq eldoc-idle-delay 0.4))

(use-package js
  :ensure nil
  :mode ("\\.jsx?\\'" . js-mode)
  :config (setq js-indent-level 2))

(use-package xref
  :ensure nil
  :config
  (define-key prog-mode-map (kbd "s-b") 'xref-find-definitions)
  (define-key prog-mode-map (kbd "s-[") 'xref-pop-marker-stack))

(use-package cc-vars
  :ensure nil
  :config
  (setq-default c-basic-offset 2)
  (setq c-default-style '((java-mode . "java")
                          (awk-mode . "awk")
                          (other . "k&r"))))

(use-package prolog
  :ensure nil
  :mode (("\\.pl\\'" . prolog-mode)))

(use-package python
  :ensure nil
  :config (setq python-indent-offset 2))

(use-package mwheel
  :ensure nil
  :config (setq mouse-wheel-scroll-amount '(1 ((shift) . 1))
                mouse-wheel-progressive-speed nil))

(use-package paren
  :ensure nil
  :config
  (setq show-paren-delay 0)
  (show-paren-mode +1))

(use-package frame
  :ensure nil
  :config
  (setq initial-frame-alist (quote ((fullscreen . maximized))))
  (blink-cursor-mode -1)
  (set-frame-font "source code pro-13:weight=regular" nil t))

(use-package ediff
  :ensure nil
  :config (setq ediff-split-window-function 'split-window-horizontally))

(use-package faces
  :ensure nil
  :config
  (defun ian/disable-bold-and-fringe-bg-face-globally ()
    "disable bold face and fringe backgroung in Emacs"
    (interactive)
    (set-face-attribute 'fringe nil :background nil)
    (mapc (lambda (face)
            (when (eq (face-attribute face :weight) 'bold)
              (set-face-attribute face nil :weight 'normal))) (face-list)))
  (add-hook 'after-init-hook 'ian/disable-bold-and-fringe-bg-face-globally))

(use-package flyspell
  :ensure nil
  :diminish flyspell-mode
  :config (setq ispell-program-name "/usr/local/bin/aspell"))

(use-package elec-pair
  :ensure nil
  :config (add-hook 'prog-mode-hook 'electric-pair-mode))

(use-package whitespace
  :ensure nil
  :config (add-hook 'before-save-hook 'whitespace-cleanup))

(use-package display-line-numbers
  :ensure nil
  :bind ("s-j" . global-display-line-numbers-mode))

;;; Third-party Packages

(use-package doom-themes :config (load-theme 'doom-dracula t))

(use-package solaire-mode
  :hook (((change-major-mode after-revert ediff-prepare-buffer) . turn-on-solaire-mode)
         (minibuffer-setup . solaire-mode-in-minibuffer))
  :config
  (solaire-global-mode)
  (solaire-mode-swap-bg))

(use-package diminish :demand t)

(use-package evil
  :diminish undo-tree-mode
  :init
  (setq evil-want-C-u-scroll t)
  (setq evil-shift-width 2)
  :hook (after-init . evil-mode)
  :config
  (with-eval-after-load 'evil-maps ; avoid conflict with company tooltip selection
    (define-key evil-insert-state-map (kbd "C-n") nil)
    (define-key evil-insert-state-map (kbd "C-p") nil))
  (evil-set-initial-state 'term-mode 'emacs)
  (defun ian/save-and-kill-this-buffer ()
    (interactive)
    (save-buffer)
    (kill-this-buffer))
  (evil-ex-define-cmd "q" 'kill-this-buffer)
  (evil-ex-define-cmd "wq" 'ian/save-and-kill-this-buffer)
  (use-package evil-commentary
    :after evil
    :diminish evil-commentary-mode
    :config (evil-commentary-mode +1)))

(use-package company
  :diminish company-mode
  :hook (prog-mode . company-mode)
  :config
  (setq company-minimum-prefix-length 1
        company-idle-delay 0.1
        company-selection-wrap-around t
        company-tooltip-align-annotations t
        company-frontends '(company-pseudo-tooltip-frontend ; show tooltip even for single candidate
                            company-echo-metadata-frontend))
  (with-eval-after-load 'company
    (define-key company-active-map (kbd "C-n") 'company-select-next)
    (define-key company-active-map (kbd "C-p") 'company-select-previous)))

(use-package flycheck
  :hook (after-init . global-flycheck-mode)
  :config (setq flycheck-python-flake8-executable "python3"))

(use-package ido-vertical-mode
  :hook ((after-init . ido-mode)
         (after-init . ido-vertical-mode))
  :config
  (setq ido-everywhere t
        ido-enable-flex-matching t
        ido-vertical-define-keys 'C-n-C-p-up-and-down))

(use-package flx-ido :config (flx-ido-mode +1))

(use-package magit :bind ("C-x g" . magit-status))

(use-package org
  :hook ((org-mode . visual-line-mode)
         (org-mode . org-indent-mode))
  :config
  (with-eval-after-load 'org
    (define-key org-mode-map (kbd "C-<tab>") nil))
  (use-package org-bullets :hook (org-mode . org-bullets-mode)))

(use-package highlight-numbers :hook (prog-mode . highlight-numbers-mode))
(use-package highlight-operators :hook (prog-mode . highlight-operators-mode))
(use-package highlight-escape-sequences :hook (prog-mode . hes-mode))

(use-package which-key
  :diminish which-key-mode
  :defer 1
  :config
  (which-key-mode +1)
  (setq which-key-idle-delay 0.4
        which-key-idle-secondary-delay 0.4))

(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-startup-banner 'logo
        dashboard-banner-logo-title "Dangerously powerful"
        dashboard-items nil
        dashboard-set-footer nil))

(use-package yasnippet-snippets
  :config
  (yas-global-mode +1)
  (advice-add 'company-complete-common
              :before
              (lambda ()
                (setq my-company-point (point))))
  (advice-add 'company-complete-common
              :after
              (lambda ()
                (when (equal my-company-point (point))
                  (yas-expand)))))

(use-package markdown-mode :hook (markdown-mode . visual-line-mode))
(use-package kotlin-mode)
(use-package dart-mode)
(use-package json-mode)

(use-package format-all
  :config
  (defun ian/format-code ()
    "Auto-format whole buffer"
    (interactive)
    (format-all-buffer)))

(use-package exec-path-from-shell
  :config (when (memq window-system '(mac ns x))
            (exec-path-from-shell-initialize)))

(use-package highlight-symbol
  :diminish highlight-symbol-mode
  :hook (prog-mode . highlight-symbol-mode)
  :config (setq highlight-symbol-idle-delay 0.3))

(use-package lsp-mode
  :hook ((c-mode ; clangd
          c-or-c++-mode ; clangd
          java-mode ; eclipse-jdtls
          js-mode ; typescript-language-server
          python-mode ; pyls
          dart-mode
          web-mode) . lsp)
  :commands lsp
  :config
  (setq lsp-prefer-flymake nil)
  (setq lsp-enable-symbol-highlighting nil)
  (use-package lsp-java :after lsp))

(use-package company-lsp
  :commands company-lsp
  :config (setq company-lsp-cache-candidates 'auto))

(use-package web-mode
  :mode (("\\.tsx?\\'" . web-mode)
         ("\\.html?\\'" . web-mode))
  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-css-indent-offset 2))

(use-package all-the-icons :config (setq all-the-icons-scale-factor 1.0))

(use-package centaur-tabs
  :demand
  :init (setq centaur-tabs-set-bar 'over)
  :config
  (centaur-tabs-mode +1)
  (centaur-tabs-headline-match)
  (setq centaur-tabs-set-modified-marker t
        centaur-tabs-modified-marker " ● "
        centaur-tabs-cycle-scope 'tabs
        centaur-tabs-height 30
        centaur-tabs-set-icons t
        centaur-tabs-close-button " × ")
  (centaur-tabs-change-fonts "Arial" 130)
  (centaur-tabs-group-by-projectile-project)
  :bind
  ("C-S-<tab>" . centaur-tabs-backward)
  ("C-<tab>" . centaur-tabs-forward))

(use-package emmet-mode
  :hook ((html-mode . emmet-mode)
         (css-mode . emmet-mode)
         (js-mode . emmet-mode)
         (web-mode . emmet-mode))
  :config (setq emmet-expand-jsx-className? t))

(use-package projectile
  :diminish projectile-mode
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "s-p") 'projectile-find-file)
  (setq projectile-sort-order 'recentf
        projectile-indexing-method 'hybrid)
  (projectile-mode +1))

(use-package smart-mode-line
  :config
  (setq sml/no-confirm-load-theme t)
  (sml/setup))

;; Fix "failed to download gnu archive" bug in Emacs 26.2
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

(provide 'init)
;;; init.el ends here
