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

  (server-start)
  (setq ring-bell-function 'ignore)
  (setq confirm-kill-processes nil)
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (setq scroll-margin 0
        scroll-conservatively 10000
        scroll-preserve-screen-position t
        auto-window-vscroll nil)
  (blink-cursor-mode t)
  (setq blink-cursor-blinks 0)
  (setq show-paren-delay 0)
  (show-paren-mode 1)
  (setq make-backup-files nil)
  (setq-default indicate-empty-lines t)
  (setq-default line-spacing 3)
  (setq frame-title-format '("Emacs")
        initial-frame-alist (quote ((fullscreen . maximized))))
  (set-frame-font "Menlo-13" nil t)
  (add-hook 'prog-mode-hook 'electric-pair-mode)
  (add-hook 'before-save-hook 'whitespace-cleanup)
  (setq auto-revert-interval 2)
  (setq auto-revert-check-vc-info t)
  (add-hook 'after-init-hook 'global-auto-revert-mode)

  (defun ian/load-init()
    "Reload `.emacs.d/init.el'."
    (interactive)
    (load-file "~/.emacs.d/init.el"))

  (defun ian/split-and-follow-horizontally ()
    "Split below."
    (interactive)
    (split-window-below)
    (other-window 1))
  (global-set-key (kbd "C-x 2") 'ian/split-and-follow-horizontally)
  (defun ian/split-and-follow-vertically ()
    "Split right."
    (interactive)
    (split-window-right)
    (other-window 1))
  (global-set-key (kbd "C-x 3") 'ian/split-and-follow-vertically)

  (setq-default indent-tabs-mode nil
                tab-width 4)
  (setq js-indent-level 2)
  (setq c-default-style
        '((java-mode . "java")
          (awk-mode . "awk")
          (other . "k&r")))
  (setq-default c-basic-offset 4)

  (defun ian/newline-indent-and-maybe-push-brace ()
    "`newline-and-indent', but bracket aware."
    (interactive)
    (insert "\n")
    (when (looking-at "}")
      (insert "\n")
      (indent-according-to-mode)
      (forward-line -1))
    (indent-according-to-mode))
  (global-set-key (kbd "RET") 'ian/newline-indent-and-maybe-push-brace)

  (use-package doom-themes
    :config (load-theme 'doom-tomorrow-night t))

  (use-package evil
    :hook (after-init . evil-mode)
    :init (setq evil-want-C-u-scroll t)
    :config
    (evil-set-initial-state 'term-mode 'emacs)
    (evil-ex-define-cmd "q" 'kill-this-buffer)

    (defun ian/save-and-kill-this-buffer ()
      (interactive)
      (save-buffer)
      (kill-this-buffer))
    (evil-ex-define-cmd "wq" 'ian/save-and-kill-this-buffer))

  (use-package company
    :hook (prog-mode . company-mode)
    :config
    (setq company-minimum-prefix-length 2
          company-idle-delay 0.1)
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

  (use-package highlight-operators
    :hook (prog-mode . highlight-operators-mode))

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
          dashboard-banner-logo-title "Welcome to Emacs. Happy Hacking!"
          dashboard-items nil
          dashboard-set-footer nil))

  (use-package yasnippet-snippets
    :config
    (yas-global-mode)
    (advice-add 'company-complete-common
                :before
                (lambda ()
                  (setq my-company-point (point))))
    (advice-add 'company-complete-common
                :after
                (lambda ()
                  (when (equal my-company-point (point))
                    (yas-expand)))))

  (use-package markdown-mode
    :hook (markdown-mode . visual-line-mode))

  (use-package shell-pop
    :bind (("C-`" . shell-pop))
    :config
    (setq shell-pop-shell-type (quote ("ansi-term" "*ansi-term*" (lambda nil (ansi-term shell-pop-term-shell)))))
    (setq shell-pop-term-shell "/bin/zsh")
    (shell-pop--set-shell-type 'shell-pop-shell-type shell-pop-shell-type))

  (use-package exec-path-from-shell
    :config
    (when (memq window-system '(mac ns x))
      (exec-path-from-shell-initialize)))

  (use-package format-all
    :config
    (defun ian/format-code()
      "Auto-format whole buffer"
      (interactive)
      (format-all-buffer)))
  ) ;; file-name-handler-alist ENDS HERE

(setq gc-cons-threshold 16777216
      gc-cons-percentage 0.1)

(provide 'init)
;;; init.el ends here
