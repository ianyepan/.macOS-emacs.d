;;; init.el --- Emacs init file
;;  Author: Ian Y.E. Pan
;;; Commentary:
;;; A use-package lightweight Emacs config containing only the essentials.
;;; Code:

(let ((file-name-handler-alist nil))

  (setq gc-cons-threshold 402653184
        gc-cons-percentage 0.6)

  (require 'package)
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
  (setq package-enable-at-startup nil)
  (package-initialize)

  (setq custom-file "~/.emacs.d/package-selected-packages.el")

  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))
  (require 'use-package-ensure)
  (setq use-package-always-ensure t)

  (setq ring-bell-function 'ignore
        confirm-kill-processes nil
        make-backup-files nil)
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (scroll-bar-mode 1)
  (fringe-mode '(nil . 0))
  (column-number-mode)
  (setq scroll-margin 0
        scroll-conservatively 10000
        scroll-preserve-screen-position t
        auto-window-vscroll nil)
  (setq blink-cursor-blinks 0
        show-paren-delay 0)
  (show-paren-mode)
  (setq-default indicate-empty-lines t)
  (setq frame-title-format '("Emacs")
        initial-frame-alist (quote ((fullscreen . maximized))))
  (set-frame-font "Source Code Pro-13" nil t)
  (setq-default line-spacing 3)
  (add-hook 'prog-mode-hook 'electric-pair-mode)
  (add-hook 'before-save-hook 'whitespace-cleanup)
  (define-key prog-mode-map (kbd "s-b") 'xref-find-definitions)
  (define-key prog-mode-map (kbd "s-[") 'xref-pop-marker-stack)
  (setq auto-revert-interval 2
        auto-revert-check-vc-info t)
  (add-hook 'after-init-hook 'global-auto-revert-mode)
  (setq-default indent-tabs-mode nil
                tab-width 4
                c-basic-offset 4)
  (setq c-default-style '((java-mode . "java")
                          (awk-mode . "awk")
                          (other . "k&r")))

  (defun ian/load-init()
    "Reload `.emacs.d/init.el'."
    (interactive)
    (load-file "~/.emacs.d/init.el"))

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
  (global-set-key (kbd "C-x 3") 'ian/split-and-follow-vertically)

  (defun ian/newline-indent-and-maybe-push-brace ()
    "insert newline and indent, but bracket aware."
    (interactive)
    (newline)
    (when (looking-at "}")
      (newline-and-indent)
      (forward-line -1))
    (indent-according-to-mode))

  (global-set-key (kbd "RET") 'ian/newline-indent-and-maybe-push-brace)

  ;; (use-package doom-themes
  ;; :config (load-theme 'doom-tomorrow-night t))

  (use-package zenburn-theme
    :config (load-theme 'zenburn t))

  (use-package evil
    :init (setq evil-want-C-u-scroll t)
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
    (evil-ex-define-cmd "wq" 'ian/save-and-kill-this-buffer))

  (use-package company
    :hook (prog-mode . company-mode)
    :config
    (setq company-minimum-prefix-length 1
          company-idle-delay 0
          company-selection-wrap-around t
          company-tooltip-align-annotations t
          company-frontends '(company-pseudo-tooltip-frontend ; show tooltip even when single candidate
                              company-echo-metadata-frontend))
    (with-eval-after-load 'company
      (define-key company-active-map (kbd "C-n") 'company-select-next)
      (define-key company-active-map (kbd "C-p") 'company-select-previous)))

  (use-package flycheck
    :hook (after-init . global-flycheck-mode)
    :config
    (setq ispell-program-name "/usr/local/bin/aspell")
    (setq flycheck-python-flake8-executable "python3"))

  (use-package ido-vertical-mode
    :hook
    (after-init . ido-mode)
    (after-init . ido-vertical-mode)
    :config
    (setq ido-everywhere t
          ido-enable-flex-matching t
          ido-vertical-define-keys 'C-n-C-p-up-and-down))

  (use-package magit
    :bind ("C-x g" . magit-status))

  (use-package org-bullets
    :hook
    (org-mode . org-bullets-mode)
    (org-mode . visual-line-mode)
    (org-mode . org-indent-mode))

  (use-package ranger
    :defer t
    :config (setq ranger-width-preview 0.5))

  (use-package highlight-numbers
    :hook (prog-mode . highlight-numbers-mode))

  ;; (use-package highlight-operators
  ;; :hook (prog-mode . highlight-operators-mode))

  (use-package highlight-escape-sequences
    :hook (prog-mode . hes-mode))

  (use-package which-key
    :defer 1
    :config
    (which-key-mode)
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
    (yas-global-mode)
    (advice-add 'company-complete-common :before (lambda () (setq my-company-point (point))))
    (advice-add 'company-complete-common :after (lambda () (when (equal my-company-point (point)) (yas-expand)))))

  (use-package markdown-mode :hook (markdown-mode . visual-line-mode))

  (use-package kotlin-mode)

  (use-package json-mode)

  (use-package format-all
    :config
    (defun ian/format-code()
      "Auto-format whole buffer"
      (interactive)
      (format-all-buffer)))

  (use-package exec-path-from-shell
    :config (when (memq window-system '(mac ns x))
              (exec-path-from-shell-initialize)))

  (use-package highlight-symbol
    :hook (prog-mode . highlight-symbol-mode)
    :config
    (setq highlight-symbol-idle-delay 0.3))

  (use-package eglot
    ;; :hook
    ;; (c-mode . eglot-ensure)
    ;; (c-or-c++-mode . eglot-ensure)
    ;; (python-mode . eglot-ensure)
    ;; (rjsx-mode . eglot-ensure)
    :config
    (setq eglot-ignored-server-capabilites (quote (:documentHighlightProvider))))

  (use-package lsp-mode
    :hook
    (c-mode . lsp)
    (c-orc++-mode . lsp)
    (java-mode . lsp)
    (python-mode . lsp)
    :commands lsp
    :config (setq lsp-enable-symbol-highlighting nil))

  (use-package company-lsp :commands (company-lsp))

  (use-package lsp-java :after lsp)

  (use-package rjsx-mode
    :hook (rjsx-mode . (lambda () (setq js-indent-level 2)))
    :config
    (add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode))
    (add-to-list 'auto-mode-alist '("\\.jsx\\'" . rjsx-mode))
    (setq js2-strict-missing-semi-warning nil))

  (use-package tide
    :after (rjsx-mode company flycheck)
    :hook (rjsx-mode . tide-setup)
    :config
    (define-key rjsx-mode-map (kbd "s-b") 'tide-jump-to-definition)
    (define-key rjsx-mode-map (kbd "s-[") 'tide-jump-back)
    (define-key typescript-mode-map (kbd "s-b") 'tide-jump-to-definition)
    (define-key typescript-mode-map (kbd "s-[") 'tide-jump-back))

  (use-package web-mode
    :mode ("\\.html\\'" . web-mode)
    :config
    (setq web-mode-markup-indent-offset 2
          web-mode-code-indent-offset 2
          web-mode-css-indent-offset 2
          web-mode-enable-auto-pairing t
          web-mode-enable-auto-expanding t
          web-mode-enable-css-colorization t))


  ) ;; file-name-handler-alist ENDS HERE

(setq gc-cons-threshold 16777216
      gc-cons-percentage 0.1)

(provide 'init)
;;; init.el ends here
