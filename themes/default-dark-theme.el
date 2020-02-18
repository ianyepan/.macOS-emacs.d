;;; default-dark-theme.el --- The default dark theme that came with Emacs

;;; Commentary:
;;
;; This theme strives to have as few lines as possible, only
;; tweaking the colors that are hinder readability in the
;; default dark theme

;;; Usage:
;;
;; (add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
;; (load-theme 'default-dark t)

;;; Code:
(deftheme default-dark)

(custom-theme-set-faces
 'default-dark
 '(default                        ((t (:background "#111111" :foreground "#eeeeee"))))
 '(company-scrollbar-bg           ((t (:background "gray20"))))
 '(company-scrollbar-fg           ((t (:background "gray50"))))
 '(company-tooltip                ((t (:inherit default :background "gray20"))))
 '(company-tooltip-common         ((t (:inherit font-lock-constant-face))))
 '(company-tooltip-annotation     ((t (:inherit font-lock-builtin-face))))
 '(company-tooltip-selection      ((t (:inherit highlight))))
 '(ivy-current-match              ((t (:inherit highlight))))
 '(vterm-color-blue               ((t (:foreground "dodgerblue"))))
 '(hl-todo                        ((t (:inverse-video t))))
 '(git-gutter-fr:modified         ((t (:foreground "skyblue"))))
 '(flycheck-posframe-warning-face ((t (:inherit warning))))
 '(flycheck-posframe-error-face   ((t (:inherit error))))
 '(erc-prompt-face                ((t (:foreground "skyblue" :background nil :box t))))
 )

(provide-theme 'default-dark)
;;; default-dark-theme.el ends here
