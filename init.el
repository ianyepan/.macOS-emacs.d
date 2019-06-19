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

  (server-start)
  (setq ring-bell-function 'ignore)
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
  (setq frame-title-format '("Emacs"))
  (setq initial-frame-alist (quote ((fullscreen . maximized))))
  (set-frame-font "Menlo-13" nil t)
  (add-hook 'prog-mode-hook 'electric-pair-mode)
  (add-hook 'before-save-hook 'whitespace-cleanup)
  (set-register ?e '(file . "~/.emacs.d/init.el"))

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
  (setq-default tab-width 4)
  (setq-default indent-tabs-mode nil)
  (setq js-indent-level 2)
  (setq c-default-style
        '((java-mode . "java")
          (awk-mode . "awk")
          (other . "k&r")))
  (setq-default c-basic-offset 4)

  (defun ian/newline-and-push-brace ()
    "`newline-and-indent', but bracket aware."
    (interactive)
    (insert "\n")
    (when (looking-at "}")
      (insert "\n")
      (indent-according-to-mode)
      (forward-line -1))
    (indent-according-to-mode))
  (global-set-key (kbd "RET") 'ian/newline-and-push-brace)

  (use-package doom-themes
    :ensure t
    :config (load-theme 'doom-tomorrow-night t))

  (use-package evil
    :ensure t
    :defer t
    :hook (after-init . evil-mode)
    :init (setq evil-want-C-u-scroll t)
    :config
    (evil-ex-define-cmd "q" 'kill-this-buffer)

    (defun ian/save-and-kill-this-buffer ()
      (interactive)
      (save-buffer)
      (kill-this-buffer))
    (evil-ex-define-cmd "wq" 'ian/save-and-kill-this-buffer))

  (use-package company
    :ensure t
    :defer t
    :hook (prog-mode . company-mode)
    :config
    (setq company-minimum-prefix-length 2)
    (setq company-idle-delay 0)
    (with-eval-after-load 'company
      (define-key company-active-map (kbd "C-n") 'company-select-next)
      (define-key company-active-map (kbd "C-p") 'company-select-previous)))

  (use-package flycheck
    :ensure t
    :defer 1
    :config
    (global-flycheck-mode)
    (setq ispell-program-name "/usr/local/bin/aspell"))

  (use-package ido-vertical-mode
    :ensure t
    :defer t
    :hook
    (after-init . ido-mode)
    (after-init . ido-vertical-mode)
    :config
    (setq ido-everywhere t)
    (setq ido-enable-flex-matching t)
    (setq ido-vertical-define-keys 'C-n-C-p-up-and-down))

  (use-package magit
    :ensure t
    :defer t
    :bind ("C-x g" . magit-status))

  (use-package org-bullets
    :ensure t
    :defer t
    :hook
    (org-mode . org-bullets-mode)
    (org-mode . visual-line-mode)
    (org-mode . org-indent-mode))

  (use-package ranger
    :ensure t
    :defer t
    :config (setq ranger-width-preview 0.5))

  (use-package highlight-numbers
    :ensure t
    :defer t
    :hook (prog-mode . highlight-numbers-mode))

  (use-package highlight-operators
    :ensure t
    :defer t
    :hook (prog-mode . highlight-operators-mode))

  (use-package highlight-escape-sequences
    :ensure t
    :defer t
    :hook (prog-mode . hes-mode))

  (use-package which-key
    :ensure t
    :defer 1
    :config
    (which-key-mode)
    (setq which-key-idle-delay 0.4)
    (setq which-key-idle-secondary-delay 0.4))

  (use-package dashboard
    :ensure t
    :config
    (dashboard-setup-startup-hook)
    (setq dashboard-startup-banner 'logo
          dashboard-banner-logo-title "Welcome to Emacs. Happy Hacking!"
          dashboard-items nil
          dashboard-set-footer nil))

  (use-package yasnippet-snippets
    :ensure t
    :defer t
    :hook (prog-mode . yas-global-mode)
    :config
    (advice-add 'company-complete-common :before
                (lambda ()
                  (setq my-company-point (point))))
    (advice-add 'company-complete-common :after
                (lambda ()
                  (when (equal my-company-point (point))
                    (yas-expand)))))

  (use-package markdown-mode
    :ensure t
    :defer t
    :hook (markdown-mode . visual-line-mode))

  ) ;; file-name-handler-alist ENDS HERE

(setq gc-cons-threshold 16777216
      gc-cons-percentage 0.1)

(provide 'init)
;;; init.el ends here
