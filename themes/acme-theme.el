;;; acme-theme.el --- A color theme for Emacs based on Acme & Sam from Plan 9

;; Copyright (C) 2020 Ian Yi-En Pan

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Credits:
;; This theme was modified from plan9-theme.el by John Louis Del Rosario

;;; Code:

(deftheme acme "Theme inspired by Acme")

;;; Color Palette

(defvar acme-colors-alist
  '(("bg"            . "#FFFFE8")
    ("bg-alt"        . "#FFFFD8")
    ("bg-alt2"       . "#eFeFd8")
    ("bg-dark"       . "#E5E5D0")
    ("fg"            . "#000000")
    ("fg-alt"        . "#B8B09A")
    ("fg-alt-dark"   . "#988d6d")
    ("fg-light"      . "#CCCCB7")
    ("highlight"     . "#e8eb98")
    ("cyan"          . "#007777")
    ("cyan-light"    . "#98ece8")
    ("red"           . "#880000")
    ("red-light"     . "#f8e8e8")
    ("yellow"        . "#888838")
    ("yellow-light"  . "#f8fce8")
    ("green"         . "#005500")
    ("green-light"   . "#e8fce8")
    ("blue"          . "#004488")
    ("blue-light"    . "#e1faff")
    ("purple"        . "#555598")
    ("purple-light"  . "#ffeaff")))

(defmacro acme/with-color-variables (&rest body)
  "`let' bind all colors defined in `acme-colors-alist' around BODY.
Also bind `class' to ((class color) (min-colors 89))."
  (declare (indent 0))
  `(let ((class '((class color) (min-colors 89)))
         ,@(mapcar (lambda (cons)
                     (list (intern (car cons)) (cdr cons)))
                   acme-colors-alist))
     ,@body))

;;; Theme Faces
(acme/with-color-variables
  (custom-theme-set-faces
   'acme

;;;; Built-in

;;;;; basic coloring
   '(button                                       ((t (:underline t))))
   `(link                                         ((t (:foreground ,blue :underline t :weight normal))))
   `(link-visited                                 ((t (:foreground ,purple :underline t :weight normal))))
   `(default                                      ((t (:foreground ,fg :background ,bg))))
   `(cursor                                       ((t (:foreground ,bg :background ,fg))))
   `(escape-glyph                                 ((t (:foreground ,cyan-light :bold nil))))
   `(fringe                                       ((t (:foreground ,fg :background ,bg))))
   `(line-number                                  ((t (:foreground ,fg :background ,bg-alt2))))
   `(line-number-current-line                     ((t (:foreground ,fg :background ,bg-alt2))))
   `(header-line                                  ((t (:foreground ,fg :background ,cyan-light :box t))))
   `(highlight                                    ((t (:background ,highlight))))
   `(success                                      ((t (:foreground ,green :weight normal))))
   `(warning                                      ((t (:foreground ,red :weight normal))))

;;;;; compilation
   `(compilation-column-face                      ((t (:foreground ,yellow :background ,yellow-light))))
   `(compilation-column-number                    ((t (:foreground ,yellow :background ,yellow-light))))
   `(compilation-error-face                       ((t (:foreground ,red :weight normal :underline t))))
   `(compilation-face                             ((t (:foreground ,fg))))
   `(compilation-info-face                        ((t (:foreground ,blue))))
   `(compilation-info                             ((t (:foreground ,blue :underline t))))
   `(compilation-line-face                        ((t (:foreground ,purple))))
   `(compilation-line-number                      ((t (:foreground ,yellow :background ,yellow-light))))
   `(compilation-message-face                     ((t (:foreground ,blue))))
   `(compilation-warning-face                     ((t (:foreground ,yellow :weight normal :underline t))))
   `(compilation-mode-line-exit                   ((t (:foreground ,cyan :weight normal))))
   `(compilation-mode-line-fail                   ((t (:foreground ,red :weight normal))))
   `(compilation-mode-line-run                    ((t (:foreground ,purple :weight normal))))

;;;;; grep
   `(grep-context-face                            ((t (:foreground ,fg-alt))))
   `(grep-error-face                              ((t (:foreground ,red :weight normal :underline t))))
   `(grep-hit-face                                ((t (:foreground ,purple :weight normal))))
   `(grep-match-face                              ((t (:foreground ,cyan :weight normal))))
   `(match                                        ((t (:background ,cyan :foreground ,cyan-light))))

;;;;; ag
   `(ag-hit-face                                  ((t (:foreground ,green :weight normal))))
   `(ag-match-face                                ((t (:foreground ,cyan :background ,cyan-light :weight normal))))

;;;;; isearch
   `(isearch                                      ((t (:foreground ,fg :weight normal :background ,cyan-light))))
   `(isearch-fail                                 ((t (:foreground ,fg :weight normal :background ,red))))
   `(lazy-highlight                               ((t (:foreground ,fg :weight normal :background ,blue-light))))

   `(menu                                         ((t (:foreground ,bg :background ,fg))))
   `(minibuffer-prompt                            ((t (:foreground ,fg :weight normal))))
   `(region                                       ((,class (:background ,highlight :foreground ,fg)) (t :inverse-video t)))
   `(secondary-selection                          ((t (:background ,green-light))))
   `(trailing-whitespace                          ((t (:background ,red-light))))
   `(vertical-border                              ((t (:foreground ,cyan))))

;;;;; font lock
   `(font-lock-builtin-face                       ((t (:foreground ,fg :weight normal))))
   `(font-lock-function-name-face                 ((t (:foreground ,fg :weight normal))))
   `(font-lock-string-face                        ((t (:foreground ,red))))
   `(font-lock-keyword-face                       ((t (:foreground ,blue :weight bold)))) ; if, else, for, while, return...
   `(font-lock-type-face                          ((t (:foreground ,fg :weight bold)))) ; int, float, string, void...
   `(font-lock-constant-face                      ((t (:foreground ,fg :weight bold)))) ; NULL, nullptr, true, false...
   `(font-lock-comment-face                       ((t (:foreground ,green :italic nil))))
   `(font-lock-comment-delimiter-face             ((t (:foreground ,green :italic nil))))
   `(font-lock-doc-face                           ((t (:foreground ,yellow :italic t))))
   `(font-lock-negation-char-face                 ((t (:foreground ,red :weight normal))))
   `(font-lock-preprocessor-face                  ((t (:foreground ,green :weight normal))))
   `(font-lock-regexp-grouping-construct          ((t (:foreground ,red :weight normal))))
   `(font-lock-regexp-grouping-backslash          ((t (:foreground ,red :weight normal))))
   `(font-lock-variable-name-face                 ((t (:foreground ,fg))))
   `(font-lock-warning-face                       ((t (:foreground ,red :weight normal))))

 ;;;; table
   `(table-cell                                   ((t (:background ,bg-alt))))

 ;;;; ledger
   `(ledger-font-directive-face                   ((t (:foreground ,cyan))))
   `(ledger-font-periodic-xact-face               ((t (:inherit ledger-font-directive-face))))
   `(ledger-font-posting-account-face             ((t (:foreground ,blue))))
   `(ledger-font-posting-amount-face              ((t (:foreground ,red))))
   `(ledger-font-posting-date-face                ((t (:foreground ,red :weight normal))))
   `(ledger-font-payee-uncleared-face             ((t (:foreground ,purple))))
   `(ledger-font-payee-cleared-face               ((t (:foreground ,fg))))
   `(ledger-font-payee-pending-face               ((t (:foreground ,yellow))))
   `(ledger-font-xact-highlight-face              ((t (:background ,bg-alt))))

;;;; Third-party


;;;;; anzu
   `(anzu-mode-line                               ((t (:foreground ,yellow :background ,yellow-light :weight normal))))

;;;;; clojure-mode
   `(clojure-interop-method-face                  ((t (:inherit font-lock-function-name-face))))

;;;;; clojure-test-mode
   `(clojure-test-failure-face                    ((t (:foreground ,red :weight normal :underline t))))
   `(clojure-test-error-face                      ((t (:foreground ,red :weight normal :underline t))))
   `(clojure-test-success-face                    ((t (:foreground ,green :weight normal :underline t))))

;;;;; diff
   `(diff-added                                   ((,class (:foreground ,fg :background ,green-light))
                                                   (t (:foreground ,fg :background ,green-light))))
   `(diff-changed                                 ((t (:foreground ,yellow))))
   `(diff-context                                 ((t (:foreground ,fg))))
   `(diff-removed                                 ((,class (:foreground ,fg :background ,red-light))
                                                   (t (:foreground ,fg :background ,red-light))))
   `(diff-refine-added                            ((t :inherit diff-added :background ,green-light :weight normal)))
   `(diff-refine-change                           ((t :inherit diff-changed :weight normal)))
   `(diff-refine-removed                          ((t :inherit diff-removed :background ,red-light :weight normal)))
   `(diff-header                                  ((,class (:foreground ,fg :weight normal))
                                                   (t (:foreground ,purple-light :weight normal))))
   `(diff-file-header                             ((,class (:foreground ,fg :background ,cyan-light :weight normal))
                                                   (t (:foreground ,fg :background ,cyan-light :weight normal))))
   `(diff-hunk-header                             ((,class (:foreground ,green :weight normal))
                                                   (t (:foreground ,green :weight normal))))

;;;;; diff-hl
   `(diff-hl-insert                               ((t (:foreground ,fg :background ,green-light))))
   `(diff-hl-delete                               ((t (:foreground ,fg :background ,red-light))))
   `(diff-hl-change                               ((t (:foreground ,fg :background ,yellow-light))))

;;;;; dired/dired+/dired-subtree
   `(dired-directory                              ((t (:foreground ,blue :weight bold))))
   `(diredp-display-msg                           ((t (:foreground ,blue))))
   `(diredp-compressed-file-suffix                ((t (:foreground ,purple))))
   `(diredp-date-time                             ((t (:foreground ,green))))
   `(diredp-deletion                              ((t (:foreground ,red))))
   `(diredp-deletion-file-name                    ((t (:foreground ,red))))
   `(diredp-dir-heading                           ((t (:foreground ,blue :background ,blue-light :weight bold))))
   `(diredp-dir-priv                              ((t (:foreground ,blue))))
   `(diredp-exec-priv                             ((t (:foreground ,yellow))))
   `(diredp-executable-tag                        ((t (:foreground ,yellow))))
   `(diredp-file-name                             ((t (:foreground ,fg))))
   `(diredp-file-suffix                           ((t (:foreground ,yellow))))
   `(diredp-flag-mark                             ((t (:foreground ,cyan))))
   `(diredp-flag-mark-line                        ((t (:foreground ,cyan))))
   `(diredp-ignored-file-name                     ((t (:foreground ,fg-light))))
   `(diredp-link-priv                             ((t (:foreground ,purple))))
   `(diredp-mode-line-flagged                     ((t (:foreground ,yellow))))
   `(diredp-mode-line-marked                      ((t (:foreground ,yellow))))
   `(diredp-no-priv                               ((t (:foreground ,fg))))
   `(diredp-number                                ((t (:foreground ,blue))))
   `(diredp-other-priv                            ((t (:foreground ,fg))))
   `(diredp-rare-priv                             ((t (:foreground ,fg))))
   `(diredp-read-priv                             ((t (:foreground ,fg))))
   `(diredp-symlink                               ((t (:foreground ,fg :background ,blue-light))))
   `(diredp-write-priv                            ((t (:foreground ,fg))))
   `(diredp-dir-name                              ((t (:foreground ,blue :weight bold))))
   `(dired-subtree-depth-1-face                   ((t (:background ,bg))))
   `(dired-subtree-depth-2-face                   ((t (:background ,bg))))
   `(dired-subtree-depth-3-face                   ((t (:background ,bg))))

;;;;; elfeed
   `(elfeed-search-date-face                      ((t (:foreground ,blue))))
   `(elfeed-search-title-face                     ((t (:foreground ,fg))))
   `(elfeed-search-unread-title-facee             ((t (:foreground ,fg))))
   `(elfeed-search-feed-face                      ((t (:foreground ,green))))
   `(elfeed-search-tag-face                       ((t (:foreground ,red))))
   `(elfeed-search-unread-count-face              ((t (:foreground ,fg))))

;;;;; erc
   `(erc-default-face                             ((t (:foreground ,fg))))
   `(erc-header-line                              ((t (:inherit header-line))))
   `(erc-action-face                              ((t (:inherit erc-default-face))))
   `(erc-bold-face                                ((t (:inherit erc-default-face :weight normal))))
   `(erc-underline-face                           ((t (:underline t))))
   `(erc-error-face                               ((t (:inherit font-lock-warning-face))))
   `(erc-prompt-face                              ((t (:foreground ,green :background ,green-light :weight normal))))
   `(erc-timestamp-face                           ((t (:foreground ,green :background ,green-light))))
   `(erc-direct-msg-face                          ((t (:inherit erc-default))))
   `(erc-notice-face                              ((t (:foreground ,fg-light))))
   `(erc-highlight-face                           ((t (:background ,highlight))))

   `(erc-input-face                               ((t (:foreground ,fg :background ,bg-alt))))
   `(erc-current-nick-face                        ((t (:foreground ,fg :background ,cyan-light :weight normal :box (:line-width 1 :style released-button)))))
   `(erc-nick-default-face                        ((t (:weight normal :background ,bg-alt))))
   `(erc-my-nick-face                             ((t (:foreground ,fg :background ,cyan-light :weight normal :box (:line-width 1 :style released-button)))))
   `(erc-nick-msg-face                            ((t (:inherit erc-default))))
   `(erc-fool-face                                ((t (:inherit erc-default))))
   `(erc-pal-face                                 ((t (:foreground ,purple :weight normal))))

   `(erc-dangerous-host-face                      ((t (:inherit font-lock-warning-face))))
   `(erc-keyword-face                             ((t (:foreground ,yellow :weight normal))))

  ;;;;; evil
   `(evil-search-highlight-persist-highlight-face ((t (:inherit lazy-highlight))))

;;;;; flx
   `(flx-highlight-face                           ((t (:foreground ,yellow :background ,green-light :weight normal :underline t))))

;;;;; company
   `(company-tooltip                              ((t (:background ,green-light))))
   `(company-tooltip-selection                    ((t (:background ,cyan-light))))
   `(company-tooltip-common-selection             ((t (:background ,cyan-light))))
   `(company-tooltip-mouse                        ((t (:background ,blue-light))))
   `(company-tooltip-search                       ((t (:foreground ,red))))
   `(company-tooltip-common                       ((t (:foreground ,red :background ,green-light))))
   `(company-tooltip-annotation                   ((t (:foreground ,green :background ,green-light))))
   `(company-scrollbar-fg                         ((t (:background ,cyan))))
   `(company-scrollbar-bg                         ((t (:background ,green-light))))
   `(company-preview                              ((t (:foreground ,fg :background ,cyan-light))))
   `(company-preview-common                       ((t (:foreground ,fg :background ,cyan-light))))

;;;;; flycheck
   `(flycheck-error                               ((((supports :underline (:style wave)))
                                                    (:underline (:style wave :color ,red) :inherit unspecified))
                                                   (t (:foreground ,red :weight normal :underline t))))
   `(flycheck-warning                             ((((supports :underline (:style wave)))
                                                    (:underline (:style wave :color ,yellow) :inherit unspecified))
                                                   (t (:foreground ,yellow :weight normal :underline t))))
   `(flycheck-info                                ((((supports :underline (:style wave)))
                                                    (:underline (:style wave :color ,purple) :inherit unspecified))
                                                   (t (:foreground ,purple :weight normal :underline t))))
   `(flycheck-fringe-error                        ((t (:foreground ,red :weight normal))))
   `(flycheck-fringe-warning                      ((t (:foreground ,yellow :weight normal))))
   `(flycheck-fringe-info                         ((t (:foreground ,purple :weight normal))))

;;;;; highlight-symbol
   `(highlight-symbol-face                        ((t (:background ,blue-light))))

;;;;; highlight-numbers
   `(highlight-numbers-number                     ((t (:foreground ,blue))))

;;;;; highlight-operators
   `(highlight-operators-face                     ((t (:foreground ,fg))))

;;;;; hl-line-mode
   `(hl-line-face                                 ((,class (:background ,highlight)) (t :weight normal)))

;;;;; hl-sexp
   `(hl-sexp-face                                 ((,class (:background ,bg-alt)) (t :weight normal)))

;;;;; ido-mode
   `(ido-first-match                              ((t (:foreground ,fg :weight normal))))
   `(ido-only-match                               ((t (:foreground ,fg :weight normal))))
   `(ido-subdir                                   ((t (:foreground ,blue))))
   `(ido-indicator                                ((t (:foreground ,yellow))))

;;;;; ido-vertical
   `(ido-vertical-first-match-face                ((t (:foreground ,fg :background ,cyan-light :weight normal))))
   `(ido-vertical-only-match-face                 ((t (:foreground ,red :background ,red-light :weight normal))))
   `(ido-vertical-match-face                      ((t (:foreground ,fg :background ,green-light :weight normal :underline t))))

;;;;; indent-guide
   `(indent-guide-face                            ((t (:foreground ,highlight))))

;;;;; ivy
   `(ivy-current-match                            ((t (:background ,highlight))))
   `(ivy-minibuffer-match-face-1                  ((t (:background ,bg-alt))))
   `(ivy-minibuffer-match-face-2                  ((t (:background ,cyan-light))))
   `(ivy-minibuffer-match-face-3                  ((t (:background ,purple-light))))
   `(ivy-minibuffer-match-face-3                  ((t (:background ,blue-light))))

;;;;; js2-mode
   `(js2-warning                                  ((t (:underline ,yellow))))
   `(js2-error                                    ((t (:foreground ,red :weight normal))))
   `(js2-jsdoc-tag                                ((t (:foreground ,purple))))
   `(js2-jsdoc-type                               ((t (:foreground ,blue))))
   `(js2-jsdoc-value                              ((t (:foreground ,cyan))))
   `(js2-function-param                           ((t (:foreground ,fg))))
   `(js2-external-variable                        ((t (:foreground ,cyan))))

;;;;; linum-mode
   `(linum                                        ((t (:foreground ,fg-light))))

;;;;; lsp-mode
   `(lsp-face-highlight-textual                   ((t (:background ,bg-dark))))
   `(lsp-face-highlight-read                      ((t (:background ,purple-light))))
   `(lsp-face-highlight-write                     ((t (:background ,green-light))))

;;;;; magit
   `(magit-section-heading                        ((t (:foreground ,cyan :weight normal :underline t))))
   `(magit-section-highlight                      ((t (:background ,bg-alt))))
   `(magit-section-heading-selection              ((t (:background ,highlight))))
   `(magit-filename                               ((t (:foreground ,fg))))
   `(magit-hash                                   ((t (:foreground ,yellow :weight normal))))
   `(magit-tag                                    ((t (:foreground ,purple :weight normal))))
   `(magit-refname                                ((t (:foreground ,purple :weight normal))))
   `(magit-head                                   ((t (:foreground ,green :weight normal))))

   `(magit-branch-local                           ((t (:foreground ,blue :background ,blue-light
                                                                   :weight normal))))
   `(magit-branch-remote                          ((t (:foreground ,green :background ,green-light
                                                                   :weight normal))))
   `(magit-branch-current                         ((t (:foreground ,cyan :background ,cyan-light
                                                                   :weight normal
                                                                   :box (:line-width 1 :color ,cyan)))))
   `(magit-diff-file-heading                      ((t (:foreground ,fg :weight normal))))
   `(magit-diff-file-heading-highlight            ((t (:background ,bg-alt))))
   `(magit-diff-file-heading-selection            ((t (:foreground ,red :background ,highlight))))
   `(magit-diff-hunk-heading                      ((t (:foreground ,blue :weight normal :underline t))))
   `(magit-diff-hunk-heading-highlight            ((t (:background ,blue-light))))
   `(magit-diff-added                             ((t (:foreground ,green :background ,green-light))))
   `(magit-diff-removed                           ((t (:foreground ,red :background ,red-light))))
   `(magit-diff-context                           ((t (:foreground ,fg-light :background nil))))
   `(magit-diff-added-highlight                   ((t (:foreground ,green :background ,green-light))))
   `(magit-diff-removed-highlight                 ((t (:foreground ,red :background ,red-light))))
   `(magit-diff-context-highlight                 ((t (:foreground ,fg-light :background ,bg-alt))))
   `(magit-diffstat-added                         ((t (:foreground ,green :background ,green-light :weight normal))))
   `(magit-diffstat-removed                       ((t (:foreground ,red :background ,red-light :weight normal))))
   `(magit-log-author                             ((t (:foreground ,blue :weight normal))))
   `(magit-log-date                               ((t (:foreground ,purple :weight normal))))
   `(magit-log-graph                              ((t (:foreground ,red :weight normal))))
   `(magit-blame-heading                          ((t (:foreground ,fg-light :background ,bg-alt))))

;;;;; paren-face
   `(parenthesis                                  ((t (:foreground "#CCCCB7"))))

;;;;; project-explorer
   `(pe/file-face                                 ((t (:foreground ,fg))))
   `(pe/directory-face                            ((t (:foreground ,blue :weight normal))))

;;;;; rainbow-delimiters
   `(rainbow-delimiters-depth-1-face              ((t (:foreground ,green))))
   `(rainbow-delimiters-depth-2-face              ((t (:foreground ,blue))))
   `(rainbow-delimiters-depth-3-face              ((t (:foreground ,red))))

;;;;; show-paren
   `(show-paren-mismatch                          ((t (:foreground ,yellow :background ,red :weight normal))))
   `(show-paren-match                             ((t (:foreground ,fg :background ,cyan-light :weight normal))))

;;;;; mode-line/sml-mode-line
   `(mode-line                                    ((,class (:foreground ,fg :background ,blue-light :box t))))
   `(mode-line-inactive                           ((t (:foreground ,fg :background ,bg-dark :box t))))
   `(mode-line-buffer-id                          ((t (:foreground ,fg :weight bold)))) ; associated buffer/file name
   `(sml/global                                   ((t (:foreground ,fg))))
   `(sml/modes                                    ((t (:foreground ,green :background ,green-light))))
   `(sml/filename                                 ((t (:foreground ,red))))
   `(sml/folder                                   ((t (:foreground ,fg))))
   `(sml/prefix                                   ((t (:foreground ,fg))))
   `(sml/read-only                                ((t (:foreground ,fg))))
   `(sml/modified                                 ((t (:foreground ,red :weight normal))))
   `(sml/outside-modified                         ((t (:background ,red :foreground ,red-light :weight normal))))
   `(sml/line-number                              ((t (:foreground ,fg :weight normal))))
   `(sml/col-number                               ((t (:foreground ,fg :weight normal))))
   `(sml/vc                                       ((t (:foreground ,fg :weight normal))))
   `(sml/vc-edited                                ((t (:foreground ,red :weight normal))))
   `(sml/git                                      ((t (:foreground ,fg :weight normal))))

;;;;; sh
   `(sh-heredoc-face                              ((t (:foreground ,purple))))

;;;;; web-mode
   `(web-mode-builtin-face                        ((t (:inherit ,font-lock-builtin-face))))
   `(web-mode-comment-face                        ((t (:inherit ,font-lock-comment-face))))
   `(web-mode-constant-face                       ((t (:inherit ,font-lock-constant-face))))
   `(web-mode-doctype-face                        ((t (:inherit ,font-lock-comment-face))))
   `(web-mode-folded-face                         ((t (:underline t))))
   `(web-mode-function-name-face                  ((t (:foreground ,fg :weight normal))))
   `(web-mode-html-attr-name-face                 ((t (:foreground ,fg))))
   `(web-mode-html-attr-value-face                ((t (:inherit ,font-lock-string-face))))
   `(web-mode-html-tag-face                       ((t (:foreground ,blue))))
   `(web-mode-keyword-face                        ((t (:inherit ,font-lock-keyword-face))))
   `(web-mode-preprocessor-face                   ((t (:inherit ,font-lock-preprocessor-face))))
   `(web-mode-string-face                         ((t (:inherit ,font-lock-string-face))))
   `(web-mode-type-face                           ((t (:inherit ,font-lock-type-face))))
   `(web-mode-variable-name-face                  ((t (:inherit ,font-lock-variable-name-face))))
   `(web-mode-server-background-face              ((t (:background ,green-light))))
   `(web-mode-server-comment-face                 ((t (:inherit web-mode-comment-face))))
   `(web-mode-server-string-face                  ((t (:foreground ,red))))
   `(web-mode-symbol-face                         ((t (:inherit font-lock-constant-face))))
   `(web-mode-warning-face                        ((t (:inherit font-lock-warning-face))))
   `(web-mode-whitespaces-face                    ((t (:background ,red-light))))
   `(web-mode-block-face                          ((t (:background ,green-light))))
   `(web-mode-current-element-highlight-face      ((t (:foreground ,fg :background ,blue-light))))

;;;;; which-func-mode
   `(which-func                                   ((t (:foreground ,purple :background ,purple-light))))

;;;;; yascroll
   `(yascroll:thumb-text-area                     ((t (:background ,highlight))))
   `(yascroll:thumb-fringe                        ((t (:background ,bg :foreground ,bg :box (:line-width 1 :style released-button)))))

;;;;; Org
   `(org-level-1                                  ((t (:height 1.4 :weight normal :background ,bg-dark))))
   `(org-level-2                                  ((t (:height 1.2 :background ,bg-dark))))
   `(org-level-3                                  ((t (:weight normal))))
   `(org-level-4                                  ((t (:foreground ,fg-alt-dark :weight normal))))
   `(org-level-5                                  ((t (:foreground ,fg-alt-dark))))
   `(org-level-6                                  ((t (:foreground ,fg-alt))))
   `(org-level-7                                  ((t (:foreground ,fg-alt))))
   `(org-level-8                                  ((t (:foreground ,fg-alt))))
   `(org-meta-line                                ((t (:foreground ,green))))
   `(org-document-info                            ((t (:foreground ,cyan :weight normal))))
   `(org-document-info-keyword                    ((t (:foreground ,cyan))))
   `(org-document-title                           ((t (:foreground ,fg :height 1.5 :weight normal :family "Sans Serif" :underline t))))
   `(org-todo                                     ((t (:foreground ,yellow :background ,bg-alt :weight normal :box (:line-width 1 :style released-button)))))
   `(org-done                                     ((t (:foreground ,green :background ,green-light :weight normal :box (:style released-button)))))
   `(org-date                                     ((t (:foreground ,purple))))
   `(org-table                                    ((t (:foreground ,purple))))
   `(org-formula                                  ((t (:foreground ,blue :background ,bg-alt))))
   `(org-code                                     ((t (:foreground ,red :background ,bg-alt))))
   `(org-verbatim                                 ((t (:foreground ,fg :background ,bg-alt :underline t))))
   `(org-special-keyword                          ((t (:foreground ,cyan))))
   `(org-agenda-date                              ((t (:foreground ,cyan))))
   `(org-agenda-structure                         ((t (:foreground ,purple))))
   `(org-block                                    ((t (:foreground ,red))))
   `(org-block-background                         ((t (:background ,bg-alt))))
   `(org-block-begin-line                         ((t (:foreground ,fg-alt :background ,bg-dark :italic t))))
   `(org-block-end-line                           ((t (:foreground ,fg-alt :background ,bg-dark :italic t))))

;;;;; origami
   `(origami-fold-replacement-face                ((t (:foreground ,red :background ,red-light
                                                                   :box (:line-width -1)))))

;;;;; git-gutter
   `(git-gutter:added                             ((t (:foreground ,green :weight normal))))
   `(git-gutter:deleted                           ((t (:foreground ,red :weight normal))))
   `(git-gutter:modified                          ((t (:foreground ,blue :weight normal))))
   `(git-gutter-fr:added                          ((t (:foreground ,green :weight normal))))
   `(git-gutter-fr:deleted                        ((t (:foreground ,red :weight normal))))
   `(git-gutter-fr:modified                       ((t (:foreground ,blue :weight normal))))

;;;;; mu4e, mail
   `(mu4e-header-highlight-face                   ((t (:background ,highlight))))
   `(mu4e-unread-face                             ((t (:foreground ,blue :weight normal))))
   `(mu4e-flagged-face                            ((t (:foreground ,red :background ,red-light :weight normal))))
   `(mu4e-compose-separator-face                  ((t (:foreground ,green))))
   `(mu4e-header-value-face                       ((t (:foreground ,fg))))
   `(message-header-name                          ((t (:foreground ,purple :weight normal))))
   `(message-header-to                            ((t (:foreground ,blue))))
   `(message-header-subject                       ((t (:foreground ,blue))))
   `(message-header-other                         ((t (:foreground ,blue))))
   `(message-cited-text                           ((t (:inherit font-lock-comment-face))))
   ))

;;; Theme Variables
(acme/with-color-variables
  (custom-theme-set-variables
   'acme

;;;;; fill-column-indicator
   `(fci-rule-color ,yellow-light)

;;;;; highlight-parentheses
   `(hl-paren-colors '(,green ,blue ,red))
   `(hl-paren-background-colors '(,green-light ,blue-light ,red-light))

;;;;; sml-mode-line
   `(sml/active-foreground-color ,fg)
   `(sml/active-background-color ,blue-light)
   `(sml/inactive-foreground-color ,fg)
   `(sml/inactive-background-color ,blue)
   ))


;;; Footer

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide-theme 'acme)

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; End:
;;; acme-theme.el ends here
