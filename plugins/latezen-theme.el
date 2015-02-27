;;; latezen-theme.el --- A low contrast color theme for Emacs.

;; Copyright (C) 2011-2013 Bozhidar Batsov

;; Author: Bozhidar Batsov <bozhidar@batsov.com>
;; URL: http://github.com/bbatsov/latezen-emacs
;; Version: 20140104.1137
;; X-Original-Version: 2.1

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

;; A port of the popular Vim theme Latezen for Emacs 24, built on top
;; of the new built-in theme support in Emacs 24.

;;; Credits:

;; Jani Nurminen created the original theme for vim on such this port
;; is based.

;;; Code:

(deftheme latezen "The Latezen color theme")

;;; Color Palette

(defvar latezen-colors-alist
  '(("latezen-fg+1"     . "#999999")
    ("latezen-fg"       . "#939491")
    ("latezen-fg-1"     . "#656555")
    ("latezen-bg-2"     . "#000000")
    ("latezen-bg-1"     . "#2B2B2B")
    ("latezen-bg-05"    . "#383838")
    ("latezen-bg"       . "#111111")
    ("latezen-bg+1"     . "#101010")
    ("latezen-bg+2"     . "#242424")
    ("latezen-bg+3"     . "#515151")
    ("latezen-error"    . "tomato")
    ("latezen-red+2"    . "#8b6969")
    ("latezen-red+1"    . "#cdba96")
    ("latezen-red"      . "#cdb5cd")
    ("latezen-string"   . "salmon")
    ("latezen-red-1"    . "#ffe4c4")
    ("latezen-red-2"    . "#ffdab9")
    ("latezen-red-3"    . "#c0c0c0")
    ("latezen-red-4"    . "#ffe4b5")
    ("latezen-orange"   . "#b0b0b0")
    ("latezen-yellow"   . "#eeeeee")
    ("latezen-yellow-1" . "#f5fffa")
    ("latezen-yellow-2" . "#cdc8b1")
    ("latezen-green-1"  . "#666666")
    ("latezen-green"    . "#9bcd9b")
    ("latezen-green+1"  . "#AAAAAA")
    ("latezen-green+2"  . "#555555")
    ("latezen-green+3"  . "#cdc9c9")
    ("latezen-green+4"  . "#B9B9B9")
    ("latezen-cyan"     . "#e6e6fa")
    ("latezen-blue+1"   . "#cdc5bf")
    ("latezen-blue"     . "#68838b")
    ("latezen-blue-1"   . "#C0C0C0")
    ("latezen-blue-2"   . "#eee0e5")
    ("latezen-blue-3"   . "#eed5d2")
    ("latezen-blue-4"   . "#cdcdb4")
    ("latezen-blue-5"   . "#ffe7ba")
    ("latezen-magenta"  . "#DC8CC3"))
  "List of Latezen colors.
Each element has the form (NAME . HEX).

`+N' suffixes indicate a color is lighter.
`-N' suffixes indicate a color is darker.")

(defmacro latezen-with-color-variables (&rest body)
  "`let' bind all colors defined in `latezen-colors-alist' around BODY.
Also bind `class' to ((class color) (min-colors 89))."
  (declare (indent 0))
  `(let ((class '((class color) (min-colors 89)))
         ,@(mapcar (lambda (cons)
                     (list (intern (car cons)) (cdr cons)))
                   latezen-colors-alist))
     ,@body))

;;; Theme Faces
(latezen-with-color-variables
  (custom-theme-set-faces
   'latezen
;;;; Built-in
;;;;; basic coloring
   '(button ((t (:underline t))))
   `(link ((t (:foreground ,latezen-blue  :underline t :weight bold))))
   `(link-visited ((t (:foreground ,latezen-yellow-2 :underline t :weight normal))))
   `(default ((t (:foreground ,latezen-fg :background ,latezen-bg))))
   `(cursor ((t (:foreground ,latezen-fg :background ,latezen-fg+1))))
   `(escape-glyph ((t (:foreground ,latezen-yellow :bold t))))
   `(fringe ((t (:foreground ,latezen-fg :background ,latezen-bg))))
   `(header-line ((t (:foreground ,latezen-yellow
                                  :background ,latezen-bg-1
                                  :box (:line-width -1 :style released-button)))))
   `(highlight ((t (:background ,latezen-bg-05))))
   `(error ((t (:foreground ,latezen-error))))
   `(success ((t (:foreground ,latezen-green :weight bold))))
   `(warning ((t (:foreground ,latezen-orange :weight bold))))
;;;;; compilation
   `(compilation-column-face ((t (:foreground ,latezen-yellow))))
   `(compilation-enter-directory-face ((t (:foreground ,latezen-green))))
   `(compilation-error-face ((t (:foreground ,latezen-red-1 :weight bold :underline t))))
   `(compilation-face ((t (:foreground ,latezen-fg))))
   `(compilation-info-face ((t (:foreground ,latezen-blue))))
   `(compilation-info ((t (:foreground ,latezen-green+4 :underline t))))
   `(compilation-leave-directory-face ((t (:foreground ,latezen-green))))
   `(compilation-line-face ((t (:foreground ,latezen-yellow))))
   `(compilation-line-number ((t (:foreground ,latezen-yellow))))
   `(compilation-message-face ((t (:foreground ,latezen-blue))))
   `(compilation-warning-face ((t (:foreground ,latezen-orange :weight bold :underline t))))
   `(compilation-mode-line-exit ((t (:foreground ,latezen-green :weight bold))))
   `(compilation-mode-line-fail ((t (:inherit error :weight bold))))
   `(compilation-mode-line-run ((t (:foreground ,latezen-yellow :weight bold))))
;;;;; grep
   `(grep-context-face ((t (:foreground ,latezen-fg))))
   `(grep-error-face ((t (:foreground ,latezen-red-1 :weight bold :underline t))))
   `(grep-hit-face ((t (:foreground ,latezen-blue))))
   `(grep-match-face ((t (:foreground ,latezen-orange :weight bold))))
   `(match ((t (:background ,latezen-bg-1 :foreground ,latezen-orange :weight bold))))
;;;;; isearch
   `(isearch ((t (:foreground ,latezen-yellow-2 :weight bold :background ,latezen-bg-1))))
   `(isearch-fail ((t (:foreground ,latezen-fg :background ,latezen-red-4))))
   `(lazy-highlight ((t (:foreground ,latezen-yellow-2 :weight bold :background ,latezen-bg-05))))

   `(menu ((t (:foreground ,latezen-fg :background ,latezen-bg))))
   `(minibuffer-prompt ((t (:foreground ,latezen-yellow))))
   `(mode-line
     ((,class (:foreground ,latezen-green+1
                           :background ,latezen-bg-1
                           :box (:line-width 2 :color ,latezen-orange)))
                           ;; :box (:line-width -1 :style released-button)))
      (t :inverse-video t)))
   `(mode-line-buffer-id ((t (:foreground ,latezen-yellow :weight bold))))
   `(mode-line-inactive
     ((t (:foreground ,latezen-green-1
                      :background ,latezen-bg-05
                      :box (:line-width 2 :color ,latezen-bg-05)))))
                      ;; :box (:line-width -1 :style released-button)))))
   `(region ((,class (:background ,latezen-bg-1))
             (t :inverse-video t)))
   `(secondary-selection ((t (:background ,latezen-bg+2))))
   `(trailing-whitespace ((t (:background ,latezen-red))))
   `(vertical-border ((t (:foreground ,latezen-fg))))
;;;;; font lock
   `(font-lock-builtin-face ((t (:foreground ,latezen-fg :weight bold))))
   `(font-lock-comment-face ((t (:foreground ,latezen-bg+3))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,latezen-green-1))))
   `(font-lock-constant-face ((t (:foreground ,latezen-green+4))))
   `(font-lock-doc-face ((t (:foreground ,latezen-bg+3))))
   `(font-lock-function-name-face ((t (:foreground ,latezen-cyan))))
   `(font-lock-keyword-face ((t (:foreground ,latezen-yellow :weight bold))))
   `(font-lock-negation-char-face ((t (:foreground ,latezen-yellow :weight bold))))
   `(font-lock-preprocessor-face ((t (:foreground ,latezen-blue+1))))
   `(font-lock-regexp-grouping-construct ((t (:foreground ,latezen-yellow :weight bold))))
   `(font-lock-regexp-grouping-backslash ((t (:foreground ,latezen-green :weight bold))))
   `(font-lock-string-face ((t (:foreground ,latezen-string))))
   `(font-lock-type-face ((t (:foreground ,latezen-blue-1))))
   `(font-lock-variable-name-face ((t (:foreground ,latezen-orange))))
   `(font-lock-warning-face ((t (:foreground ,latezen-yellow-2 :weight bold))))

   `(c-annotation-face ((t (:inherit font-lock-constant-face))))
;;;;; newsticker
   `(newsticker-date-face ((t (:foreground ,latezen-fg))))
   `(newsticker-default-face ((t (:foreground ,latezen-fg))))
   `(newsticker-enclosure-face ((t (:foreground ,latezen-green+3))))
   `(newsticker-extra-face ((t (:foreground ,latezen-bg+2 :height 0.8))))
   `(newsticker-feed-face ((t (:foreground ,latezen-fg))))
   `(newsticker-immortal-item-face ((t (:foreground ,latezen-green))))
   `(newsticker-new-item-face ((t (:foreground ,latezen-blue))))
   `(newsticker-obsolete-item-face ((t (:foreground ,latezen-red))))
   `(newsticker-old-item-face ((t (:foreground ,latezen-bg+3))))
   `(newsticker-statistics-face ((t (:foreground ,latezen-fg))))
   `(newsticker-treeview-face ((t (:foreground ,latezen-fg))))
   `(newsticker-treeview-immortal-face ((t (:foreground ,latezen-green))))
   `(newsticker-treeview-listwindow-face ((t (:foreground ,latezen-fg))))
   `(newsticker-treeview-new-face ((t (:foreground ,latezen-blue :weight bold))))
   `(newsticker-treeview-obsolete-face ((t (:foreground ,latezen-red))))
   `(newsticker-treeview-old-face ((t (:foreground ,latezen-bg+3))))
   `(newsticker-treeview-selection-face ((t (:background ,latezen-bg-1 :foreground ,latezen-yellow))))
;;;; Third-party
;;;;; ace-jump
   `(ace-jump-face-background
     ((t (:foreground ,latezen-fg-1 :background ,latezen-bg :inverse-video nil))))
   `(ace-jump-face-foreground
     ((t (:foreground ,latezen-green+2 :background ,latezen-bg :inverse-video nil))))
;;;;; anzu
   `(anzu-mode-line ((t (:foreground ,latezen-cyan :weight bold))))
;;;;; full-ack
   `(ack-separator ((t (:foreground ,latezen-fg))))
   `(ack-file ((t (:foreground ,latezen-blue))))
   `(ack-line ((t (:foreground ,latezen-yellow))))
   `(ack-match ((t (:foreground ,latezen-orange :background ,latezen-bg-1 :weight bold))))
;;;;; auctex
   `(font-latex-bold-face ((t (:inherit bold))))
   `(font-latex-warning-face ((t (:foreground nil :inherit font-lock-warning-face))))
   `(font-latex-sectioning-5-face ((t (:foreground ,latezen-red :weight bold ))))
   `(font-latex-sedate-face ((t (:foreground ,latezen-yellow))))
   `(font-latex-italic-face ((t (:foreground ,latezen-cyan :slant italic))))
   `(font-latex-string-face ((t (:inherit ,font-lock-string-face))))
   `(font-latex-math-face ((t (:foreground ,latezen-orange))))
;;;;; company
   `(company-tooltip ((t (:inherit default :background ,latezen-bg+3 :foreground ,latezen-yellow))))
   `(company-scrollbar-bg ((t (:background ,latezen-green-1))))
   `(company-scrollbar-fg ((t (:background ,latezen-green+1))))
   `(company-tooltip-selection ((t (:background ,latezen-green+3 :foreground ,latezen-bg-05))))
   `(company-tooltip-common ((t (:background ,latezen-bg+3 :foreground ,latezen-yellow))))   
   `(company-tooltip-common-selection ((t (:background ,latezen-green+3 :foreground ,latezen-bg-05))))   
   `(company-preview-common ((t (:background ,latezen-bg-05 ))))
   `(company-tooltip-annotation ((t (:background ,latezen-bg+3 :foreground ,latezen-yellow ))))
;;;;; auto-complete
   `(ac-candidate-face ((t (:background ,latezen-bg+3 :foreground ,latezen-red+1))))
   `(ac-selection-face ((t (:background ,latezen-blue-4 :foreground ,latezen-bg-05))))
   `(ac-gtags-candidate-face ((t (:inherit 'ac-candidate-face))))
   `(ac-gtags-selection-face ((t (:inherit 'ac-selection-face))))
   `(ac-yasnippet-candidate-face ((t (:inherit 'ac-candidate-face))))
   ;; `(ac-yasnippet-selection-face ((t (:inherit 'ac-selection-face))))
   `(popup-tip-face ((t (:background ,latezen-yellow-2 :foreground ,latezen-bg-2))))
   `(popup-scroll-bar-foreground-face ((t (:background ,latezen-blue-5))))
   `(popup-scroll-bar-background-face ((t (:background ,latezen-bg-1))))
   `(popup-isearch-match ((t (:background ,latezen-bg :foreground ,latezen-fg))))
;;;;; android mode
   `(android-mode-debug-face ((t (:foreground ,latezen-green+1))))
   `(android-mode-error-face ((t (:foreground ,latezen-orange :weight bold))))
   `(android-mode-info-face ((t (:foreground ,latezen-fg))))
   `(android-mode-verbose-face ((t (:foreground ,latezen-green))))
   `(android-mode-warning-face ((t (:foreground ,latezen-yellow))))
;;;;; bm
   `(bm-face ((t (:background ,latezen-yellow-1 :foreground ,latezen-bg))))
   `(bm-fringe-face ((t (:background ,latezen-yellow-1 :foreground ,latezen-bg))))
   `(bm-fringe-persistent-face ((t (:background ,latezen-green-1 :foreground ,latezen-bg))))
   `(bm-persistent-face ((t (:background ,latezen-green-1 :foreground ,latezen-bg))))
;;;;; clojure-test-mode
   `(clojure-test-failure-face ((t (:foreground ,latezen-orange :weight bold :underline t))))
   `(clojure-test-error-face ((t (:foreground ,latezen-red :weight bold :underline t))))
   `(clojure-test-success-face ((t (:foreground ,latezen-green+1 :weight bold :underline t))))
;;;;; coq
   `(coq-solve-tactics-face ((t (:foreground nil :inherit font-lock-constant-face))))
;;;;; ctable
   `(ctbl:face-cell-select ((t (:background ,latezen-blue :foreground ,latezen-bg))))
   `(ctbl:face-continue-bar ((t (:background ,latezen-bg-05 :foreground ,latezen-bg))))
   `(ctbl:face-row-select ((t (:background ,latezen-cyan :foreground ,latezen-bg))))
;;;;; diff
   `(diff-added ((,class (:foreground ,latezen-green :background nil))
                 (t (:foreground ,latezen-green-1 :background nil))))
   `(diff-changed ((t (:foreground ,latezen-yellow))))
   `(diff-removed ((,class (:foreground ,latezen-red+2 :background nil))
                   (t (:foreground ,latezen-red-3 :background nil))))
   `(diff-refine-added ((t :inherit diff-added :weight bold)))
   `(diff-refine-change ((t :inherit diff-changed :weight bold)))
   `(diff-refine-removed ((t :inherit diff-removed :weight bold)))
   `(diff-header ((,class (:background ,latezen-bg+2))
                  (t (:background ,latezen-fg :foreground ,latezen-bg))))
   `(diff-file-header
     ((,class (:background ,latezen-bg+2 :foreground ,latezen-fg :bold t))
      (t (:background ,latezen-fg :foreground ,latezen-bg :bold t))))
;;;;; dired+
   `(diredp-display-msg ((t (:foreground ,latezen-blue))))
   `(diredp-compressed-file-suffix ((t (:foreground ,latezen-orange))))
   `(diredp-date-time ((t (:foreground ,latezen-magenta))))
   `(diredp-deletion ((t (:foreground ,latezen-yellow))))
   `(diredp-deletion-file-name ((t (:foreground ,latezen-red))))
   `(diredp-dir-heading ((t (:foreground ,latezen-blue :background ,latezen-bg-1))))
   `(diredp-dir-priv ((t (:foreground ,latezen-cyan))))
   `(diredp-exec-priv ((t (:foreground ,latezen-red))))
   `(diredp-executable-tag ((t (:foreground ,latezen-green+1))))
   `(diredp-file-name ((t (:foreground ,latezen-blue))))
   `(diredp-file-suffix ((t (:foreground ,latezen-green))))
   `(diredp-flag-mark ((t (:foreground ,latezen-yellow))))
   `(diredp-flag-mark-line ((t (:foreground ,latezen-orange))))
   `(diredp-ignored-file-name ((t (:foreground ,latezen-red))))
   `(diredp-link-priv ((t (:foreground ,latezen-yellow))))
   `(diredp-mode-line-flagged ((t (:foreground ,latezen-yellow))))
   `(diredp-mode-line-marked ((t (:foreground ,latezen-orange))))
   `(diredp-no-priv ((t (:foreground ,latezen-fg))))
   `(diredp-number ((t (:foreground ,latezen-green+1))))
   `(diredp-other-priv ((t (:foreground ,latezen-yellow-1))))
   `(diredp-rare-priv ((t (:foreground ,latezen-red-1))))
   `(diredp-read-priv ((t (:foreground ,latezen-green-1))))
   `(diredp-symlink ((t (:foreground ,latezen-yellow))))
   `(diredp-write-priv ((t (:foreground ,latezen-magenta))))
;;;;; ediff
   `(ediff-current-diff-A ((t (:foreground ,latezen-fg :background ,latezen-red-4))))
   `(ediff-current-diff-Ancestor ((t (:foreground ,latezen-fg :background ,latezen-red-4))))
   `(ediff-current-diff-B ((t (:foreground ,latezen-fg :background ,latezen-green-1))))
   `(ediff-current-diff-C ((t (:foreground ,latezen-fg :background ,latezen-blue-5))))
   `(ediff-even-diff-A ((t (:background ,latezen-bg+1))))
   `(ediff-even-diff-Ancestor ((t (:background ,latezen-bg+1))))
   `(ediff-even-diff-B ((t (:background ,latezen-bg+1))))
   `(ediff-even-diff-C ((t (:background ,latezen-bg+1))))
   `(ediff-fine-diff-A ((t (:foreground ,latezen-fg :background ,latezen-red-2 :weight bold))))
   `(ediff-fine-diff-Ancestor ((t (:foreground ,latezen-fg :background ,latezen-red-2 weight bold))))
   `(ediff-fine-diff-B ((t (:foreground ,latezen-fg :background ,latezen-green :weight bold))))
   `(ediff-fine-diff-C ((t (:foreground ,latezen-fg :background ,latezen-blue-3 :weight bold ))))
   `(ediff-odd-diff-A ((t (:background ,latezen-bg+2))))
   `(ediff-odd-diff-Ancestor ((t (:background ,latezen-bg+2))))
   `(ediff-odd-diff-B ((t (:background ,latezen-bg+2))))
   `(ediff-odd-diff-C ((t (:background ,latezen-bg+2))))
;;;;; ert
   `(ert-test-result-expected ((t (:foreground ,latezen-green+4 :background ,latezen-bg))))
   `(ert-test-result-unexpected ((t (:foreground ,latezen-red :background ,latezen-bg))))
;;;;; eshell
   `(eshell-prompt ((t (:foreground ,latezen-yellow :weight bold))))
   `(eshell-ls-archive ((t (:foreground ,latezen-red-1 :weight bold))))
   `(eshell-ls-backup ((t (:inherit font-lock-comment-face))))
   `(eshell-ls-clutter ((t (:inherit font-lock-comment-face))))
   `(eshell-ls-directory ((t (:foreground ,latezen-blue+1 :weight bold))))
   `(eshell-ls-executable ((t (:foreground ,latezen-red+1 :weight bold))))
   `(eshell-ls-unreadable ((t (:foreground ,latezen-fg))))
   `(eshell-ls-missing ((t (:inherit font-lock-warning-face))))
   `(eshell-ls-product ((t (:inherit font-lock-doc-face))))
   `(eshell-ls-special ((t (:foreground ,latezen-yellow :weight bold))))
   `(eshell-ls-symlink ((t (:foreground ,latezen-cyan :weight bold))))
;;;;; flx
   `(flx-highlight-face ((t (:foreground ,latezen-green+2 :weight bold))))
;;;;; flycheck
   `(flycheck-error
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,latezen-red-1) :inherit unspecified))
      (t (:foreground ,latezen-red-1 :weight bold :underline t))))
   `(flycheck-warning
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,latezen-yellow) :inherit unspecified))
      (t (:foreground ,latezen-yellow :weight bold :underline t))))
   `(flycheck-info
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,latezen-cyan) :inherit unspecified))
      (t (:foreground ,latezen-cyan :weight bold :underline t))))
   `(flycheck-fringe-error ((t (:foreground ,latezen-red-1 :weight bold))))
   `(flycheck-fringe-warning ((t (:foreground ,latezen-yellow :weight bold))))
   `(flycheck-fringe-info ((t (:foreground ,latezen-cyan :weight bold))))
;;;;; flymake
   `(flymake-errline
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,latezen-red)
                   :inherit unspecified :foreground unspecified :background unspecified))
      (t (:foreground ,latezen-red-1 :weight bold :underline t))))
   `(flymake-warnline
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,latezen-orange)
                   :inherit unspecified :foreground unspecified :background unspecified))
      (t (:foreground ,latezen-orange :weight bold :underline t))))
   `(flymake-infoline
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,latezen-green)
                   :inherit unspecified :foreground unspecified :background unspecified))
      (t (:foreground ,latezen-green-1 :weight bold :underline t))))
;;;;; flyspell
   `(flyspell-duplicate
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,latezen-orange) :inherit unspecified))
      (t (:foreground ,latezen-orange :weight bold :underline t))))
   `(flyspell-incorrect
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,latezen-red) :inherit unspecified))
      (t (:foreground ,latezen-red-1 :weight bold :underline t))))
;;;;; erc
   `(erc-action-face ((t (:inherit erc-default-face))))
   `(erc-bold-face ((t (:weight bold))))
   `(erc-current-nick-face ((t (:foreground ,latezen-blue :weight bold))))
   `(erc-dangerous-host-face ((t (:inherit font-lock-warning-face))))
   `(erc-default-face ((t (:foreground ,latezen-fg))))
   `(erc-direct-msg-face ((t (:inherit erc-default))))
   `(erc-error-face ((t (:inherit font-lock-warning-face))))
   `(erc-fool-face ((t (:inherit erc-default))))
   `(erc-highlight-face ((t (:inherit hover-highlight))))
   `(erc-input-face ((t (:foreground ,latezen-yellow))))
   `(erc-keyword-face ((t (:foreground ,latezen-blue :weight bold))))
   `(erc-nick-default-face ((t (:foreground ,latezen-yellow :weight bold))))
   `(erc-my-nick-face ((t (:foreground ,latezen-red :weight bold))))
   `(erc-nick-msg-face ((t (:inherit erc-default))))
   `(erc-notice-face ((t (:foreground ,latezen-green))))
   `(erc-pal-face ((t (:foreground ,latezen-orange :weight bold))))
   `(erc-prompt-face ((t (:foreground ,latezen-orange :background ,latezen-bg :weight bold))))
   `(erc-timestamp-face ((t (:foreground ,latezen-green+4))))
   `(erc-underline-face ((t (:underline t))))
;;;;; git-gutter
   `(git-gutter:added ((t (:foreground ,latezen-green :weight bold :inverse-video t))))
   `(git-gutter:deleted ((t (:foreground ,latezen-red :weight bold :inverse-video t))))
   `(git-gutter:modified ((t (:foreground ,latezen-magenta :weight bold :inverse-video t))))
   `(git-gutter:unchanged ((t (:foreground ,latezen-fg :weight bold :inverse-video t))))
;;;;; git-gutter-fr
   `(git-gutter-fr:added ((t (:foreground ,latezen-green  :weight bold))))
   `(git-gutter-fr:deleted ((t (:foreground ,latezen-red :weight bold))))
   `(git-gutter-fr:modified ((t (:foreground ,latezen-magenta :weight bold))))
;;;;; gnus
   `(gnus-group-mail-1 ((t (:bold t :inherit gnus-group-mail-1-empty))))
   `(gnus-group-mail-1-empty ((t (:inherit gnus-group-news-1-empty))))
   `(gnus-group-mail-2 ((t (:bold t :inherit gnus-group-mail-2-empty))))
   `(gnus-group-mail-2-empty ((t (:inherit gnus-group-news-2-empty))))
   `(gnus-group-mail-3 ((t (:bold t :inherit gnus-group-mail-3-empty))))
   `(gnus-group-mail-3-empty ((t (:inherit gnus-group-news-3-empty))))
   `(gnus-group-mail-4 ((t (:bold t :inherit gnus-group-mail-4-empty))))
   `(gnus-group-mail-4-empty ((t (:inherit gnus-group-news-4-empty))))
   `(gnus-group-mail-5 ((t (:bold t :inherit gnus-group-mail-5-empty))))
   `(gnus-group-mail-5-empty ((t (:inherit gnus-group-news-5-empty))))
   `(gnus-group-mail-6 ((t (:bold t :inherit gnus-group-mail-6-empty))))
   `(gnus-group-mail-6-empty ((t (:inherit gnus-group-news-6-empty))))
   `(gnus-group-mail-low ((t (:bold t :inherit gnus-group-mail-low-empty))))
   `(gnus-group-mail-low-empty ((t (:inherit gnus-group-news-low-empty))))
   `(gnus-group-news-1 ((t (:bold t :inherit gnus-group-news-1-empty))))
   `(gnus-group-news-2 ((t (:bold t :inherit gnus-group-news-2-empty))))
   `(gnus-group-news-3 ((t (:bold t :inherit gnus-group-news-3-empty))))
   `(gnus-group-news-4 ((t (:bold t :inherit gnus-group-news-4-empty))))
   `(gnus-group-news-5 ((t (:bold t :inherit gnus-group-news-5-empty))))
   `(gnus-group-news-6 ((t (:bold t :inherit gnus-group-news-6-empty))))
   `(gnus-group-news-low ((t (:bold t :inherit gnus-group-news-low-empty))))
   `(gnus-header-content ((t (:inherit message-header-other))))
   `(gnus-header-from ((t (:inherit message-header-from))))
   `(gnus-header-name ((t (:inherit message-header-name))))
   `(gnus-header-newsgroups ((t (:inherit message-header-other))))
   `(gnus-header-subject ((t (:inherit message-header-subject))))
   `(gnus-summary-cancelled ((t (:foreground ,latezen-orange))))
   `(gnus-summary-high-ancient ((t (:foreground ,latezen-blue))))
   `(gnus-summary-high-read ((t (:foreground ,latezen-green :weight bold))))
   `(gnus-summary-high-ticked ((t (:foreground ,latezen-orange :weight bold))))
   `(gnus-summary-high-unread ((t (:foreground ,latezen-fg :weight bold))))
   `(gnus-summary-low-ancient ((t (:foreground ,latezen-blue))))
   `(gnus-summary-low-read ((t (:foreground ,latezen-green))))
   `(gnus-summary-low-ticked ((t (:foreground ,latezen-orange :weight bold))))
   `(gnus-summary-low-unread ((t (:foreground ,latezen-fg))))
   `(gnus-summary-normal-ancient ((t (:foreground ,latezen-blue))))
   `(gnus-summary-normal-read ((t (:foreground ,latezen-green))))
   `(gnus-summary-normal-ticked ((t (:foreground ,latezen-orange :weight bold))))
   `(gnus-summary-normal-unread ((t (:foreground ,latezen-fg))))
   `(gnus-summary-selected ((t (:foreground ,latezen-yellow :weight bold))))
   `(gnus-cite-1 ((t (:foreground ,latezen-blue))))
   `(gnus-cite-10 ((t (:foreground ,latezen-yellow-1))))
   `(gnus-cite-11 ((t (:foreground ,latezen-yellow))))
   `(gnus-cite-2 ((t (:foreground ,latezen-blue-1))))
   `(gnus-cite-3 ((t (:foreground ,latezen-blue-2))))
   `(gnus-cite-4 ((t (:foreground ,latezen-green+2))))
   `(gnus-cite-5 ((t (:foreground ,latezen-green+1))))
   `(gnus-cite-6 ((t (:foreground ,latezen-green))))
   `(gnus-cite-7 ((t (:foreground ,latezen-red))))
   `(gnus-cite-8 ((t (:foreground ,latezen-red-1))))
   `(gnus-cite-9 ((t (:foreground ,latezen-red-2))))
   `(gnus-group-news-1-empty ((t (:foreground ,latezen-yellow))))
   `(gnus-group-news-2-empty ((t (:foreground ,latezen-green+3))))
   `(gnus-group-news-3-empty ((t (:foreground ,latezen-green+1))))
   `(gnus-group-news-4-empty ((t (:foreground ,latezen-blue-2))))
   `(gnus-group-news-5-empty ((t (:foreground ,latezen-blue-3))))
   `(gnus-group-news-6-empty ((t (:foreground ,latezen-bg+2))))
   `(gnus-group-news-low-empty ((t (:foreground ,latezen-bg+2))))
   `(gnus-signature ((t (:foreground ,latezen-yellow))))
   `(gnus-x ((t (:background ,latezen-fg :foreground ,latezen-bg))))
;;;;; guide-key
   `(guide-key/highlight-command-face ((t (:foreground ,latezen-blue))))
   `(guide-key/key-face ((t (:foreground ,latezen-green))))
   `(guide-key/prefix-command-face ((t (:foreground ,latezen-green+1))))
;;;;; helm
   `(helm-header
     ((t (:foreground ,latezen-green
                      :background ,latezen-bg
                      :underline nil
                      :box nil))))
   `(helm-source-header
     ((t (:foreground ,latezen-yellow
                      :background ,latezen-bg-1
                      :underline nil
                      :weight bold
                      :box (:line-width -1 :style released-button)))))
   `(helm-selection ((t (:background ,latezen-bg+1 :underline nil))))
   `(helm-selection-line ((t (:background ,latezen-bg+1))))
   `(helm-visible-mark ((t (:foreground ,latezen-bg :background ,latezen-yellow-2))))
   `(helm-candidate-number ((t (:foreground ,latezen-green+4 :background ,latezen-bg-1))))
   `(helm-separator ((t (:foreground ,latezen-red :background ,latezen-bg))))
   `(helm-time-zone-current ((t (:foreground ,latezen-green+2 :background ,latezen-bg))))
   `(helm-time-zone-home ((t (:foreground ,latezen-red :background ,latezen-bg))))
   `(helm-bookmark-addressbook ((t (:foreground ,latezen-orange :background ,latezen-bg))))
   `(helm-bookmark-directory ((t (:foreground nil :background nil :inherit helm-ff-directory))))
   `(helm-bookmark-file ((t (:foreground nil :background nil :inherit helm-ff-file))))
   `(helm-bookmark-gnus ((t (:foreground ,latezen-magenta :background ,latezen-bg))))
   `(helm-bookmark-info ((t (:foreground ,latezen-green+2 :background ,latezen-bg))))
   `(helm-bookmark-man ((t (:foreground ,latezen-yellow :background ,latezen-bg))))
   `(helm-bookmark-w3m ((t (:foreground ,latezen-magenta :background ,latezen-bg))))
   `(helm-buffer-not-saved ((t (:foreground ,latezen-red :background ,latezen-bg))))
   `(helm-buffer-process ((t (:foreground ,latezen-cyan :background ,latezen-bg))))
   `(helm-buffer-saved-out ((t (:foreground ,latezen-fg :background ,latezen-bg))))
   `(helm-buffer-size ((t (:foreground ,latezen-fg-1 :background ,latezen-bg))))
   `(helm-ff-directory ((t (:foreground ,latezen-cyan :background ,latezen-bg :weight bold))))
   `(helm-ff-file ((t (:foreground ,latezen-fg :background ,latezen-bg :weight normal))))
   `(helm-ff-executable ((t (:foreground ,latezen-green+2 :background ,latezen-bg :weight normal))))
   `(helm-ff-invalid-symlink ((t (:foreground ,latezen-red :background ,latezen-bg :weight bold))))
   `(helm-ff-symlink ((t (:foreground ,latezen-yellow :background ,latezen-bg :weight bold))))
   `(helm-ff-prefix ((t (:foreground ,latezen-bg :background ,latezen-yellow :weight normal))))
   `(helm-grep-cmd-line ((t (:foreground ,latezen-cyan :background ,latezen-bg))))
   `(helm-grep-file ((t (:foreground ,latezen-fg :background ,latezen-bg))))
   `(helm-grep-finish ((t (:foreground ,latezen-green+2 :background ,latezen-bg))))
   `(helm-grep-lineno ((t (:foreground ,latezen-fg-1 :background ,latezen-bg))))
   `(helm-grep-match ((t (:foreground nil :background nil :inherit helm-match))))
   `(helm-grep-running ((t (:foreground ,latezen-red :background ,latezen-bg))))
   `(helm-moccur-buffer ((t (:foreground ,latezen-cyan :background ,latezen-bg))))
   `(helm-mu-contacts-address-face ((t (:foreground ,latezen-fg-1 :background ,latezen-bg))))
   `(helm-mu-contacts-name-face ((t (:foreground ,latezen-fg :background ,latezen-bg))))
;;;;; hl-line-mode
   `(hl-line-face ((,class (:background ,latezen-bg-05))
                   (t :weight bold)))
   `(hl-line ((,class (:background ,latezen-bg-05)) ; old emacsen
              (t :weight bold)))
;;;;; hl-sexp
   `(hl-sexp-face ((,class (:background ,latezen-bg+1))
                   (t :weight bold)))
;;;;; ido-mode
   `(ido-first-match ((t (:foreground ,latezen-yellow :weight bold))))
   `(ido-only-match ((t (:foreground ,latezen-orange :weight bold))))
   `(ido-subdir ((t (:foreground ,latezen-yellow))))
   `(ido-indicator ((t (:foreground ,latezen-yellow :background ,latezen-red-4))))
;;;;; iedit-mode
   `(iedit-occurrence ((t (:background ,latezen-bg+2 :weight bold))))
;;;;; js2-mode
   `(js2-warning ((t (:underline ,latezen-orange))))
   `(js2-error ((t (:foreground ,latezen-red :weight bold))))
   `(js2-jsdoc-tag ((t (:foreground ,latezen-green-1))))
   `(js2-jsdoc-type ((t (:foreground ,latezen-green+2))))
   `(js2-jsdoc-value ((t (:foreground ,latezen-green+3))))
   `(js2-function-param ((t (:foreground, latezen-green+3))))
   `(js2-external-variable ((t (:foreground ,latezen-orange))))
;;;;; jabber-mode
   `(jabber-roster-user-away ((t (:foreground ,latezen-green+2))))
   `(jabber-roster-user-online ((t (:foreground ,latezen-blue-1))))
   `(jabber-roster-user-dnd ((t (:foreground ,latezen-red+1))))
   `(jabber-rare-time-face ((t (:foreground ,latezen-green+1))))
   `(jabber-chat-prompt-local ((t (:foreground ,latezen-blue-1))))
   `(jabber-chat-prompt-foreign ((t (:foreground ,latezen-red+1))))
   `(jabber-activity-face((t (:foreground ,latezen-red+1))))
   `(jabber-activity-personal-face ((t (:foreground ,latezen-blue+1))))
   `(jabber-title-small ((t (:height 1.1 :weight bold))))
   `(jabber-title-medium ((t (:height 1.2 :weight bold))))
   `(jabber-title-large ((t (:height 1.3 :weight bold))))
;;;;; ledger-mode
   `(ledger-font-payee-uncleared-face ((t (:foreground ,latezen-red-1 :weight bold))))
   `(ledger-font-payee-cleared-face ((t (:foreground ,latezen-fg :weight normal))))
   `(ledger-font-xact-highlight-face ((t (:background ,latezen-bg+1))))
   `(ledger-font-pending-face ((t (:foreground ,latezen-orange weight: normal))))
   `(ledger-font-other-face ((t (:foreground ,latezen-fg))))
   `(ledger-font-posting-account-face ((t (:foreground ,latezen-blue-1))))
   `(ledger-font-posting-account-cleared-face ((t (:foreground ,latezen-fg))))
   `(ledger-font-posting-account-pending-face ((t (:foreground ,latezen-orange))))
   `(ledger-font-posting-amount-face ((t (:foreground ,latezen-orange))))
   `(ledger-font-posting-account-pending-face ((t (:foreground ,latezen-orange))))
   `(ledger-occur-narrowed-face ((t (:foreground ,latezen-fg-1 :invisible t))))
   `(ledger-occur-xact-face ((t (:background ,latezen-bg+1))))
   `(ledger-font-comment-face ((t (:foreground ,latezen-green))))
   `(ledger-font-reconciler-uncleared-face ((t (:foreground ,latezen-red-1 :weight bold))))
   `(ledger-font-reconciler-cleared-face ((t (:foreground ,latezen-fg :weight normal))))
   `(ledger-font-reconciler-pending-face ((t (:foreground ,latezen-orange :weight normal))))
   `(ledger-font-report-clickable-face ((t (:foreground ,latezen-orange :weight normal))))
;;;;; linum-mode
   `(linum ((t (:foreground ,latezen-green+2 :background ,latezen-bg))))
;;;;; macrostep
   `(macrostep-gensym-1
     ((t (:foreground ,latezen-green+2 :background ,latezen-bg-1))))
   `(macrostep-gensym-2
     ((t (:foreground ,latezen-red+1 :background ,latezen-bg-1))))
   `(macrostep-gensym-3
     ((t (:foreground ,latezen-blue+1 :background ,latezen-bg-1))))
   `(macrostep-gensym-4
     ((t (:foreground ,latezen-magenta :background ,latezen-bg-1))))
   `(macrostep-gensym-5
     ((t (:foreground ,latezen-yellow :background ,latezen-bg-1))))
   `(macrostep-expansion-highlight-face
     ((t (:inherit highlight))))
   `(macrostep-macro-face
     ((t (:underline t))))
;;;;; magit
   `(magit-section-title ((t (:foreground ,latezen-yellow :weight bold))))
   `(magit-branch ((t (:foreground ,latezen-orange :weight bold))))
   `(magit-item-highlight ((t (:background ,latezen-bg+1 :bold nil))))
;;;;; egg
   `(egg-text-base ((t (:foreground ,latezen-fg))))
   `(egg-help-header-1 ((t (:foreground ,latezen-yellow))))
   `(egg-help-header-2 ((t (:foreground ,latezen-green+3))))
   `(egg-branch ((t (:foreground ,latezen-yellow))))
   `(egg-branch-mono ((t (:foreground ,latezen-yellow))))
   `(egg-term ((t (:foreground ,latezen-yellow))))
   `(egg-diff-add ((t (:foreground ,latezen-green+4))))
   `(egg-diff-del ((t (:foreground ,latezen-red+1))))
   `(egg-diff-file-header ((t (:foreground ,latezen-yellow-2))))
   `(egg-section-title ((t (:foreground ,latezen-yellow))))
   `(egg-stash-mono ((t (:foreground ,latezen-green+4))))
;;;;; message-mode
   `(message-cited-text ((t (:inherit font-lock-comment-face))))
   `(message-header-name ((t (:foreground ,latezen-green+1))))
   `(message-header-other ((t (:foreground ,latezen-green))))
   `(message-header-to ((t (:foreground ,latezen-yellow :weight bold))))
   `(message-header-from ((t (:foreground ,latezen-yellow :weight bold))))
   `(message-header-cc ((t (:foreground ,latezen-yellow :weight bold))))
   `(message-header-newsgroups ((t (:foreground ,latezen-yellow :weight bold))))
   `(message-header-subject ((t (:foreground ,latezen-orange :weight bold))))
   `(message-header-xheader ((t (:foreground ,latezen-green))))
   `(message-mml ((t (:foreground ,latezen-yellow :weight bold))))
   `(message-separator ((t (:inherit font-lock-comment-face))))
;;;;; mew
   `(mew-face-header-subject ((t (:foreground ,latezen-orange))))
   `(mew-face-header-from ((t (:foreground ,latezen-yellow))))
   `(mew-face-header-date ((t (:foreground ,latezen-green))))
   `(mew-face-header-to ((t (:foreground ,latezen-red))))
   `(mew-face-header-key ((t (:foreground ,latezen-green))))
   `(mew-face-header-private ((t (:foreground ,latezen-green))))
   `(mew-face-header-important ((t (:foreground ,latezen-blue))))
   `(mew-face-header-marginal ((t (:foreground ,latezen-fg :weight bold))))
   `(mew-face-header-warning ((t (:foreground ,latezen-red))))
   `(mew-face-header-xmew ((t (:foreground ,latezen-green))))
   `(mew-face-header-xmew-bad ((t (:foreground ,latezen-red))))
   `(mew-face-body-url ((t (:foreground ,latezen-orange))))
   `(mew-face-body-comment ((t (:foreground ,latezen-fg :slant italic))))
   `(mew-face-body-cite1 ((t (:foreground ,latezen-green))))
   `(mew-face-body-cite2 ((t (:foreground ,latezen-blue))))
   `(mew-face-body-cite3 ((t (:foreground ,latezen-orange))))
   `(mew-face-body-cite4 ((t (:foreground ,latezen-yellow))))
   `(mew-face-body-cite5 ((t (:foreground ,latezen-red))))
   `(mew-face-mark-review ((t (:foreground ,latezen-blue))))
   `(mew-face-mark-escape ((t (:foreground ,latezen-green))))
   `(mew-face-mark-delete ((t (:foreground ,latezen-red))))
   `(mew-face-mark-unlink ((t (:foreground ,latezen-yellow))))
   `(mew-face-mark-refile ((t (:foreground ,latezen-green))))
   `(mew-face-mark-unread ((t (:foreground ,latezen-red-2))))
   `(mew-face-eof-message ((t (:foreground ,latezen-green))))
   `(mew-face-eof-part ((t (:foreground ,latezen-yellow))))
;;;;; mic-paren
   `(paren-face-match ((t (:foreground ,latezen-cyan :background ,latezen-bg :weight bold))))
   `(paren-face-mismatch ((t (:foreground ,latezen-bg :background ,latezen-magenta :weight bold))))
   `(paren-face-no-match ((t (:foreground ,latezen-bg :background ,latezen-red :weight bold))))
;;;;; mingus
   `(mingus-directory-face ((t (:foreground ,latezen-blue))))
   `(mingus-pausing-face ((t (:foreground ,latezen-magenta))))
   `(mingus-playing-face ((t (:foreground ,latezen-cyan))))
   `(mingus-playlist-face ((t (:foreground ,latezen-cyan ))))
   `(mingus-song-file-face ((t (:foreground ,latezen-yellow))))
   `(mingus-stopped-face ((t (:foreground ,latezen-red))))
;;;;; nav
   `(nav-face-heading ((t (:foreground ,latezen-yellow))))
   `(nav-face-button-num ((t (:foreground ,latezen-cyan))))
   `(nav-face-dir ((t (:foreground ,latezen-green))))
   `(nav-face-hdir ((t (:foreground ,latezen-red))))
   `(nav-face-file ((t (:foreground ,latezen-fg))))
   `(nav-face-hfile ((t (:foreground ,latezen-red-4))))
;;;;; mu4e
   `(mu4e-cited-1-face ((t (:foreground ,latezen-blue    :slant italic))))
   `(mu4e-cited-2-face ((t (:foreground ,latezen-green+2 :slant italic))))
   `(mu4e-cited-3-face ((t (:foreground ,latezen-blue-2  :slant italic))))
   `(mu4e-cited-4-face ((t (:foreground ,latezen-green   :slant italic))))
   `(mu4e-cited-5-face ((t (:foreground ,latezen-blue-4  :slant italic))))
   `(mu4e-cited-6-face ((t (:foreground ,latezen-green-1 :slant italic))))
   `(mu4e-cited-7-face ((t (:foreground ,latezen-blue    :slant italic))))
   `(mu4e-replied-face ((t (:foreground ,latezen-bg+3))))
   `(mu4e-trashed-face ((t (:foreground ,latezen-bg+3 :strike-through t))))
;;;;; mumamo
   `(mumamo-background-chunk-major ((t (:background nil))))
   `(mumamo-background-chunk-submode1 ((t (:background ,latezen-bg-1))))
   `(mumamo-background-chunk-submode2 ((t (:background ,latezen-bg+2))))
   `(mumamo-background-chunk-submode3 ((t (:background ,latezen-bg+3))))
   `(mumamo-background-chunk-submode4 ((t (:background ,latezen-bg+1))))
;;;;; org-mode
   `(org-agenda-date-today
     ((t (:foreground ,latezen-fg+1 :slant italic :weight bold))) t)
   `(org-agenda-structure
     ((t (:inherit font-lock-comment-face))))
   `(org-archived ((t (:foreground ,latezen-fg :weight bold))))
   `(org-checkbox ((t (:background ,latezen-bg+2 :foreground ,latezen-fg+1
                                   :box (:line-width 1 :style released-button)))))
   `(org-date ((t (:foreground ,latezen-blue :underline t))))
   `(org-deadline-announce ((t (:foreground ,latezen-red-1))))
   `(org-done ((t (:bold t :weight bold :foreground ,latezen-green))))
   `(org-formula ((t (:foreground ,latezen-yellow-2))))
   `(org-headline-done ((t (:foreground ,latezen-green+3))))
   `(org-hide ((t (:foreground ,latezen-bg-1))))
   `(org-level-1 ((t (:foreground ,latezen-blue-3))))
   `(org-level-2 ((t (:foreground ,latezen-red-3))))
   `(org-level-3 ((t (:foreground ,latezen-green+3))))
   `(org-level-4 ((t (:foreground ,latezen-yellow-2))))
   `(org-level-5 ((t (:foreground ,latezen-cyan))))
   `(org-level-6 ((t (:foreground ,latezen-green+2))))
   `(org-level-7 ((t (:foreground ,latezen-red-4))))
   `(org-level-8 ((t (:foreground ,latezen-blue-4))))
   `(org-link ((t (:foreground ,latezen-blue :underline t))))
   `(org-scheduled ((t (:foreground ,latezen-green+4))))
   `(org-scheduled-previously ((t (:foreground ,latezen-red))))
   `(org-scheduled-today ((t (:foreground ,latezen-blue+1))))
   `(org-sexp-date ((t (:foreground ,latezen-blue+1 :underline t))))
   `(org-special-keyword ((t (:inherit font-lock-comment-face))))
   `(org-table ((t (:foreground ,latezen-red+2))))
   `(org-tag ((t (:bold t :weight bold))))
   `(org-time-grid ((t (:foreground ,latezen-orange))))
   `(org-todo ((t (:bold t :foreground ,latezen-error :weight bold))))
   `(org-upcoming-deadline ((t (:inherit font-lock-keyword-face))))
   `(org-warning ((t (:bold t :foreground ,latezen-red :weight bold :underline nil))))
   `(org-column ((t (:background ,latezen-bg-1))))
   `(org-column-title ((t (:background ,latezen-bg-1 :underline t :weight bold))))
   `(org-mode-line-clock ((t (:foreground ,latezen-fg :background ,latezen-bg-1))))
   `(org-mode-line-clock-overrun ((t (:foreground ,latezen-bg :background ,latezen-red-1))))
   `(org-ellipsis ((t (:foreground ,latezen-yellow-1 :underline t))))
   `(org-footnote ((t (:foreground ,latezen-cyan :underline t))))
;;;;; outline
   `(outline-1 ((t (:foreground ,latezen-orange))))
   `(outline-2 ((t (:foreground ,latezen-green+4))))
   `(outline-3 ((t (:foreground ,latezen-blue-1))))
   `(outline-4 ((t (:foreground ,latezen-yellow-2))))
   `(outline-5 ((t (:foreground ,latezen-cyan))))
   `(outline-6 ((t (:foreground ,latezen-green+2))))
   `(outline-7 ((t (:foreground ,latezen-red-4))))
   `(outline-8 ((t (:foreground ,latezen-blue-4))))
;;;;; p4
   `(p4-depot-added-face ((t :inherit diff-added)))
   `(p4-depot-branch-op-face ((t :inherit diff-changed)))
   `(p4-depot-deleted-face ((t :inherit diff-removed)))
   `(p4-depot-unmapped-face ((t :inherit diff-changed)))
   `(p4-diff-change-face ((t :inherit diff-changed)))
   `(p4-diff-del-face ((t :inherit diff-removed)))
   `(p4-diff-file-face ((t :inherit diff-file-header)))
   `(p4-diff-head-face ((t :inherit diff-header)))
   `(p4-diff-ins-face ((t :inherit diff-added)))
;;;;; perspective
   `(persp-selected-face ((t (:foreground ,latezen-yellow-2 :inherit mode-line))))
;;;;; powerline
   `(powerline-active1 ((t (:background ,latezen-bg-05 :inherit mode-line))))
   `(powerline-active2 ((t (:background ,latezen-bg+2 :inherit mode-line))))
   `(powerline-inactive1 ((t (:background ,latezen-bg+1 :inherit mode-line-inactive))))
   `(powerline-inactive2 ((t (:background ,latezen-bg+3 :inherit mode-line-inactive))))
;;;;; proofgeneral
   `(proof-active-area-face ((t (:underline t))))
   `(proof-boring-face ((t (:foreground ,latezen-fg :background ,latezen-bg+2))))
   `(proof-command-mouse-highlight-face ((t (:inherit proof-mouse-highlight-face))))
   `(proof-debug-message-face ((t (:inherit proof-boring-face))))
   `(proof-declaration-name-face ((t (:inherit font-lock-keyword-face :foreground nil))))
   `(proof-eager-annotation-face ((t (:foreground ,latezen-bg :background ,latezen-orange))))
   `(proof-error-face ((t (:foreground ,latezen-fg :background ,latezen-red-4))))
   `(proof-highlight-dependency-face ((t (:foreground ,latezen-bg :background ,latezen-yellow-1))))
   `(proof-highlight-dependent-face ((t (:foreground ,latezen-bg :background ,latezen-orange))))
   `(proof-locked-face ((t (:background ,latezen-blue-5))))
   `(proof-mouse-highlight-face ((t (:foreground ,latezen-bg :background ,latezen-orange))))
   `(proof-queue-face ((t (:background ,latezen-red-4))))
   `(proof-region-mouse-highlight-face ((t (:inherit proof-mouse-highlight-face))))
   `(proof-script-highlight-error-face ((t (:background ,latezen-red-2))))
   `(proof-tacticals-name-face ((t (:inherit font-lock-constant-face :foreground nil :background ,latezen-bg))))
   `(proof-tactics-name-face ((t (:inherit font-lock-constant-face :foreground nil :background ,latezen-bg))))
   `(proof-warning-face ((t (:foreground ,latezen-bg :background ,latezen-yellow-1))))
;;;;; rainbow-delimiters
   `(rainbow-delimiters-depth-1-face ((t (:foreground ,latezen-fg))))
   `(rainbow-delimiters-depth-2-face ((t (:foreground ,latezen-green+4))))
   `(rainbow-delimiters-depth-3-face ((t (:foreground ,latezen-yellow-2))))
   `(rainbow-delimiters-depth-4-face ((t (:foreground ,latezen-cyan))))
   `(rainbow-delimiters-depth-5-face ((t (:foreground ,latezen-green+2))))
   `(rainbow-delimiters-depth-6-face ((t (:foreground ,latezen-blue+1))))
   `(rainbow-delimiters-depth-7-face ((t (:foreground ,latezen-yellow-1))))
   `(rainbow-delimiters-depth-8-face ((t (:foreground ,latezen-green+1))))
   `(rainbow-delimiters-depth-9-face ((t (:foreground ,latezen-blue-2))))
   `(rainbow-delimiters-depth-10-face ((t (:foreground ,latezen-orange))))
   `(rainbow-delimiters-depth-11-face ((t (:foreground ,latezen-green))))
   `(rainbow-delimiters-depth-12-face ((t (:foreground ,latezen-blue-5))))
;;;;; rcirc
   `(rcirc-my-nick ((t (:foreground ,latezen-blue))))
   `(rcirc-other-nick ((t (:foreground ,latezen-orange))))
   `(rcirc-bright-nick ((t (:foreground ,latezen-blue+1))))
   `(rcirc-dim-nick ((t (:foreground ,latezen-blue-2))))
   `(rcirc-server ((t (:foreground ,latezen-green))))
   `(rcirc-server-prefix ((t (:foreground ,latezen-green+1))))
   `(rcirc-timestamp ((t (:foreground ,latezen-green+2))))
   `(rcirc-nick-in-message ((t (:foreground ,latezen-yellow))))
   `(rcirc-nick-in-message-full-line ((t (:bold t))))
   `(rcirc-prompt ((t (:foreground ,latezen-yellow :bold t))))
   `(rcirc-track-nick ((t (:inverse-video t))))
   `(rcirc-track-keyword ((t (:bold t))))
   `(rcirc-url ((t (:bold t))))
   `(rcirc-keyword ((t (:foreground ,latezen-yellow :bold t))))
;;;;; rpm-mode
   `(rpm-spec-dir-face ((t (:foreground ,latezen-green))))
   `(rpm-spec-doc-face ((t (:foreground ,latezen-green))))
   `(rpm-spec-ghost-face ((t (:foreground ,latezen-red))))
   `(rpm-spec-macro-face ((t (:foreground ,latezen-yellow))))
   `(rpm-spec-obsolete-tag-face ((t (:foreground ,latezen-red))))
   `(rpm-spec-package-face ((t (:foreground ,latezen-red))))
   `(rpm-spec-section-face ((t (:foreground ,latezen-yellow))))
   `(rpm-spec-tag-face ((t (:foreground ,latezen-blue))))
   `(rpm-spec-var-face ((t (:foreground ,latezen-red))))
;;;;; rst-mode
   `(rst-level-1-face ((t (:foreground ,latezen-orange))))
   `(rst-level-2-face ((t (:foreground ,latezen-green+1))))
   `(rst-level-3-face ((t (:foreground ,latezen-blue-1))))
   `(rst-level-4-face ((t (:foreground ,latezen-yellow-2))))
   `(rst-level-5-face ((t (:foreground ,latezen-cyan))))
   `(rst-level-6-face ((t (:foreground ,latezen-green-1))))
;;;;; sh-mode
   `(sh-heredoc     ((t (:foreground ,latezen-yellow :bold t))))
   `(sh-quoted-exec ((t (:foreground ,latezen-red))))
;;;;; show-paren
   `(show-paren-mismatch ((t (:foreground ,latezen-red+1 :background ,latezen-bg+3 :weight bold))))
   `(show-paren-match ((t (:background ,latezen-bg+3 :weight bold))))
;;;;; smartparens
   `(sp-show-pair-mismatch-face ((t (:foreground ,latezen-red+1 :background ,latezen-bg+3 :weight bold))))
   `(sp-show-pair-match-face ((t (:background ,latezen-bg+3 :weight bold))))
;;;;; sml-mode-line
   '(sml-modeline-end-face ((t :inherit default :width condensed)))
;;;;; SLIME
   `(slime-repl-output-face ((t (:foreground ,latezen-red))))
   `(slime-repl-inputed-output-face ((t (:foreground ,latezen-green))))
   `(slime-error-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,latezen-red)))
      (t
       (:underline ,latezen-red))))
   `(slime-warning-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,latezen-orange)))
      (t
       (:underline ,latezen-orange))))
   `(slime-style-warning-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,latezen-yellow)))
      (t
       (:underline ,latezen-yellow))))
   `(slime-note-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,latezen-green)))
      (t
       (:underline ,latezen-green))))
   `(slime-highlight-face ((t (:inherit highlight))))
;;;;; speedbar
   `(speedbar-button-face ((t (:foreground ,latezen-green+2))))
   `(speedbar-directory-face ((t (:foreground ,latezen-cyan))))
   `(speedbar-file-face ((t (:foreground ,latezen-fg))))
   `(speedbar-highlight-face ((t (:foreground ,latezen-bg :background ,latezen-green+2))))
   `(speedbar-selected-face ((t (:foreground ,latezen-red))))
   `(speedbar-separator-face ((t (:foreground ,latezen-bg :background ,latezen-blue-1))))
   `(speedbar-tag-face ((t (:foreground ,latezen-yellow))))
;;;;; tabbar
   `(tabbar-button ((t (:foreground ,latezen-fg
                                    :background ,latezen-bg))))
   `(tabbar-selected ((t (:foreground ,latezen-fg
                                      :background ,latezen-bg
                                      :box (:line-width -1 :style pressed-button)))))
   `(tabbar-unselected ((t (:foreground ,latezen-fg
                                        :background ,latezen-bg+1
                                        :box (:line-width -1 :style released-button)))))
;;;;; term
   `(term-color-black ((t (:foreground ,latezen-bg
                                       :background ,latezen-bg-1))))
   `(term-color-red ((t (:foreground ,latezen-red-2
                                       :background ,latezen-red-4))))
   `(term-color-green ((t (:foreground ,latezen-green
                                       :background ,latezen-green+2))))
   `(term-color-yellow ((t (:foreground ,latezen-orange
                                       :background ,latezen-yellow))))
   `(term-color-blue ((t (:foreground ,latezen-blue-1
                                      :background ,latezen-blue-4))))
   `(term-color-magenta ((t (:foreground ,latezen-magenta
                                         :background ,latezen-red))))
   `(term-color-cyan ((t (:foreground ,latezen-cyan
                                       :background ,latezen-blue))))
   `(term-color-white ((t (:foreground ,latezen-fg
                                       :background ,latezen-fg-1))))
   '(term-default-fg-color ((t (:inherit term-color-white))))
   '(term-default-bg-color ((t (:inherit term-color-black))))
;;;;; undo-tree
   `(undo-tree-visualizer-active-branch-face ((t (:foreground ,latezen-fg+1 :weight bold))))
   `(undo-tree-visualizer-current-face ((t (:foreground ,latezen-red-1 :weight bold))))
   `(undo-tree-visualizer-default-face ((t (:foreground ,latezen-fg))))
   `(undo-tree-visualizer-register-face ((t (:foreground ,latezen-yellow))))
   `(undo-tree-visualizer-unmodified-face ((t (:foreground ,latezen-cyan))))
;;;;; volatile-highlights
   `(vhl/default-face ((t (:background ,latezen-bg-05))))
;;;;; emacs-w3m
   `(w3m-anchor ((t (:foreground ,latezen-yellow :underline t
                                 :weight bold))))
   `(w3m-arrived-anchor ((t (:foreground ,latezen-yellow-2
                                         :underline t :weight normal))))
   `(w3m-form ((t (:foreground ,latezen-red-1 :underline t))))
   `(w3m-header-line-location-title ((t (:foreground ,latezen-yellow
                                                     :underline t :weight bold))))
   '(w3m-history-current-url ((t (:inherit match))))
   `(w3m-lnum ((t (:foreground ,latezen-green+2 :background ,latezen-bg))))
   `(w3m-lnum-match ((t (:background ,latezen-bg-1
                                     :foreground ,latezen-orange
                                     :weight bold))))
   `(w3m-lnum-minibuffer-prompt ((t (:foreground ,latezen-yellow))))
;;;;; web-mode
   `(web-mode-builtin-face ((t (:inherit ,font-lock-builtin-face))))
   `(web-mode-comment-face ((t (:inherit ,font-lock-comment-face))))
   `(web-mode-constant-face ((t (:inherit ,font-lock-constant-face))))
   `(web-mode-css-at-rule-face ((t (:foreground ,latezen-orange ))))
   `(web-mode-css-prop-face ((t (:foreground ,latezen-orange))))
   `(web-mode-css-pseudo-class-face ((t (:foreground ,latezen-green+3 :weight bold))))
   `(web-mode-css-rule-face ((t (:foreground ,latezen-blue))))
   `(web-mode-doctype-face ((t (:inherit ,font-lock-comment-face))))
   `(web-mode-folded-face ((t (:underline t))))
   `(web-mode-function-name-face ((t (:foreground ,latezen-blue))))
   `(web-mode-html-attr-name-face ((t (:foreground ,latezen-orange))))
   `(web-mode-html-attr-value-face ((t (:inherit ,font-lock-string-face))))
   `(web-mode-html-tag-face ((t (:foreground ,latezen-cyan))))
   `(web-mode-keyword-face ((t (:inherit ,font-lock-keyword-face))))
   `(web-mode-preprocessor-face ((t (:inherit ,font-lock-preprocessor-face))))
   `(web-mode-string-face ((t (:inherit ,font-lock-string-face))))
   `(web-mode-type-face ((t (:inherit ,font-lock-type-face))))
   `(web-mode-variable-name-face ((t (:inherit ,font-lock-variable-name-face))))
   `(web-mode-server-background-face ((t (:background ,latezen-bg))))
   `(web-mode-server-comment-face ((t (:inherit web-mode-comment-face))))
   `(web-mode-server-string-face ((t (:inherit web-mode-string-face))))
   `(web-mode-symbol-face ((t (:inherit font-lock-constant-face))))
   `(web-mode-warning-face ((t (:inherit font-lock-warning-face))))
   `(web-mode-whitespaces-face ((t (:background ,latezen-red))))
;;;;; whitespace-mode
   `(whitespace-space ((t (:background ,latezen-bg+1 :foreground ,latezen-bg+1))))
   `(whitespace-hspace ((t (:background ,latezen-bg+1 :foreground ,latezen-bg+1))))
   `(whitespace-tab ((t (:background ,latezen-red-1))))
   `(whitespace-newline ((t (:foreground ,latezen-bg+1))))
   `(whitespace-trailing ((t (:background ,latezen-red))))
   `(whitespace-line ((t (:background ,latezen-bg :foreground ,latezen-magenta))))
   `(whitespace-space-before-tab ((t (:background ,latezen-orange :foreground ,latezen-orange))))
   `(whitespace-indentation ((t (:background ,latezen-yellow :foreground ,latezen-red))))
   `(whitespace-empty ((t (:background ,latezen-yellow))))
   `(whitespace-space-after-tab ((t (:background ,latezen-yellow :foreground ,latezen-red))))
;;;;; wanderlust
   `(wl-highlight-folder-few-face ((t (:foreground ,latezen-red-2))))
   `(wl-highlight-folder-many-face ((t (:foreground ,latezen-red-1))))
   `(wl-highlight-folder-path-face ((t (:foreground ,latezen-orange))))
   `(wl-highlight-folder-unread-face ((t (:foreground ,latezen-blue))))
   `(wl-highlight-folder-zero-face ((t (:foreground ,latezen-fg))))
   `(wl-highlight-folder-unknown-face ((t (:foreground ,latezen-blue))))
   `(wl-highlight-message-citation-header ((t (:foreground ,latezen-red-1))))
   `(wl-highlight-message-cited-text-1 ((t (:foreground ,latezen-red))))
   `(wl-highlight-message-cited-text-2 ((t (:foreground ,latezen-green+2))))
   `(wl-highlight-message-cited-text-3 ((t (:foreground ,latezen-blue))))
   `(wl-highlight-message-cited-text-4 ((t (:foreground ,latezen-blue+1))))
   `(wl-highlight-message-header-contents-face ((t (:foreground ,latezen-green))))
   `(wl-highlight-message-headers-face ((t (:foreground ,latezen-red+1))))
   `(wl-highlight-message-important-header-contents ((t (:foreground ,latezen-green+2))))
   `(wl-highlight-message-header-contents ((t (:foreground ,latezen-green+1))))
   `(wl-highlight-message-important-header-contents2 ((t (:foreground ,latezen-green+2))))
   `(wl-highlight-message-signature ((t (:foreground ,latezen-green))))
   `(wl-highlight-message-unimportant-header-contents ((t (:foreground ,latezen-fg))))
   `(wl-highlight-summary-answered-face ((t (:foreground ,latezen-blue))))
   `(wl-highlight-summary-disposed-face ((t (:foreground ,latezen-fg
                                                         :slant italic))))
   `(wl-highlight-summary-new-face ((t (:foreground ,latezen-blue))))
   `(wl-highlight-summary-normal-face ((t (:foreground ,latezen-fg))))
   `(wl-highlight-summary-thread-top-face ((t (:foreground ,latezen-yellow))))
   `(wl-highlight-thread-indent-face ((t (:foreground ,latezen-magenta))))
   `(wl-highlight-summary-refiled-face ((t (:foreground ,latezen-fg))))
   `(wl-highlight-summary-displaying-face ((t (:underline t :weight bold))))
;;;;; which-func-mode
   `(which-func ((t (:foreground ,latezen-green+4))))
;;;;; yascroll
   `(yascroll:thumb-text-area ((t (:background ,latezen-bg-1))))
   `(yascroll:thumb-fringe ((t (:background ,latezen-bg-1 :foreground ,latezen-bg-1))))
   ))

;;; Theme Variables
(latezen-with-color-variables
  (custom-theme-set-variables
   'latezen
;;;;; ansi-color
   `(ansi-color-names-vector [,latezen-bg ,latezen-red ,latezen-green ,latezen-yellow
                                          ,latezen-blue ,latezen-magenta ,latezen-cyan ,latezen-fg])
;;;;; fill-column-indicator
   `(fci-rule-color ,latezen-bg-05)
;;;;; vc-annotate
   `(vc-annotate-color-map
     '((20 . "#FFCCCC")
       (40 . "#FFD8CC")
       (60 . "#FFE4CC")
       (80 . "#FFF0CC")
       (100 . "#FFFCCC")
       (120 . "#F6FFCC")
       (140 . "#EAFFCC")
       (160 . "#DEFFCC")
       (180 . "#D2FFCC")
       (200 . "#CCFFD2")
       (220 . "#CCFFDE")
       (240 . "#CCFFEA")
       (260 . "#CCFFF6")
       (280 . "#CCFCFF")
       (300 . "#CCF0FF")
       (320 . "#CCE4FF")
       (340 . "#CCD8FF")
       (360 . "#CCCCFF")))
   `(vc-annotate-very-old-color ,latezen-magenta)
   `(vc-annotate-background ,latezen-bg-1)
   ))

;;; Rainbow Support

(declare-function rainbow-mode 'rainbow-mode)
(declare-function rainbow-colorize-by-assoc 'rainbow-mode)

(defvar latezen-add-font-lock-keywords nil
  "Whether to add font-lock keywords for latezen color names.
In buffers visiting library `latezen-theme.el' the latezen
specific keywords are always added.  In all other Emacs-Lisp
buffers this variable controls whether this should be done.
This requires library `rainbow-mode'.")

(defvar latezen-colors-font-lock-keywords nil)

;; (defadvice rainbow-turn-on (after latezen activate)
;;   "Maybe also add font-lock keywords for latezen colors."
;;   (when (and (derived-mode-p 'emacs-lisp-mode)
;;              (or latezen-add-font-lock-keywords
;;                  (equal (file-name-nondirectory (buffer-file-name))
;;                         "latezen-theme.el")))
;;     (unless latezen-colors-font-lock-keywords
;;       (setq latezen-colors-font-lock-keywords
;;             `((,(regexp-opt (mapcar 'car latezen-colors-alist) 'words)
;;                (0 (rainbow-colorize-by-assoc latezen-colors-alist))))))
;;     (font-lock-add-keywords nil latezen-colors-font-lock-keywords)))

;; (defadvice rainbow-turn-off (after latezen activate)
;;   "Also remove font-lock keywords for latezen colors."
;;   (font-lock-remove-keywords nil latezen-colors-font-lock-keywords))

;;; Footer

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide-theme 'latezen)

;;;###autoload
(add-to-list 'safe-local-eval-forms
             '(when (require 'rainbow-mode nil t) (rainbow-mode 1)))

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; eval: (when (require 'rainbow-mode nil t) (rainbow-mode 1))
;; End:
;;; latezen-theme.el ends here
