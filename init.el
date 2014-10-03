(prefer-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
;; (set-keyboard-coding-system 'utf-8)
;; (set-terminal-coding-system 'utf-8)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(custom-safe-themes (quote ("8942d5e757ed5f18efe33d2366effd5ce867c00e125f6fc12fcf2a2f5253a99f" "cf3b9e992b68532e634aebe50b74ae65373cf5054d3fc4d8c4ea5ef437d2d12b" "501caa208affa1145ccbb4b74b6cd66c3091e41c5bb66c677feda9def5eab19c" "72cc9ae08503b8e977801c6d6ec17043b55313cda34bcf0e6921f2f04cf2da56" default)))
 '(display-time-mode t)
 '(initial-buffer-choice "~/organize.org")
 '(menu-bar-mode nil)
 '(ring-bell-function (quote ignore) t)
 '(show-paren-mode t)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 `(default ((t (:inherit nil 
                         :stipple nil 
                         :inverse-video nil 
                         :box nil 
                         :strike-through nil 
                         :overline nil 
                         :underline nil 
                         :slant normal 
                         :weight normal 
                         :height 90
                         :width normal 
                         :foundry "outline" 
                         :family ,(if (eq 'windows-nt system-type)
                                     "Source Code Pro"
                                   "Fira Mono"))))))

(setq indicate-empty-lines t)
(setq system-time-locale "C")

;; (setq grep-hit-face font-lock-type-face) ;; font-lock-keyword-face)
(setq grep-hit-face  font-lock-keyword-face)

(setq column-number-mode t)
(setq nxml-slash-auto-complete-flag t)

(require 'cl)

(defalias 'yes-or-no-p 'y-or-n-p)
(setq confirm-kill-emacs 'yes-or-no-p)

(global-set-key (kbd "C-x C-b") 'ibuffer)

(global-auto-revert-mode)

(put 'dired-find-alternate-file 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'set-goal-column 'disabled nil)

(put 'upcase-region 'disabled nil)
;; (require 'dired+)
(put 'narrow-to-region 'disabled nil)
(global-subword-mode)

(define-key isearch-mode-map (kbd "C-o")
  (lambda ()
    (interactive)
    (let ((case-fold-search isearch-case-fold-search))
      (occur (if isearch-regexp isearch-string
               (regexp-quote isearch-string))))))

(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(toggle-scroll-bar -1)
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

(defun coding-utf-stuff()
  (setq locale-coding-system 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-language-environment 'UTF-8) ; prefer utf-8 for language settings
  (setq read-quoted-char-radix 10) ; use decimal, not octal
 ;; http://chopmode.wordpress.com/2009/03/04/changing-text-encoding-in-emacs/
  )

(defun three-quarters-window ()
  "Resizes current window big"
  (interactive)
  (let ((size (- (truncate (* .75 (frame-height))) (window-height))))
    (if (> size 0)
        (enlarge-window size))))

(setq backup-directory-alist
	  `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
	  `((".*" ,temporary-file-directory t)))

;; (setq-default ispell-program-name "c:/cygwin/bin/aspell")

(require 'recentf)
(recentf-mode 1)
(setq recentf-max-saved-items 500)
(setq recentf-max-menu-items 20)
(global-set-key [(meta f12)] 'recentf-open-files)

(defun xsteve-ido-choose-from-recentf ()
  "Use ido to select a recently opened file from the `recentf-list'"
  (interactive)
  (let ((home (expand-file-name (getenv "HOME"))))
    (find-file
     (ido-completing-read "Recentf open: "
			  (mapcar (lambda (path)
				    (replace-regexp-in-string home "~" path))
				  recentf-list)
			  nil t))))

(global-set-key (kbd "C-x C-r") 'xsteve-ido-choose-from-recentf)

(defun fix-endings ()
  (interactive)
  (set-buffer-process-coding-system 'undecided-unix 'undecided-unix)
)

;; (add-to-list 'load-path "~/.emacs.d/gtags/share/gtags")
;; (require 'gtags)

(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

(delete-selection-mode t)
;; (cua-selection-mode t)
;; (setq cua-auto-tabify-rectangles nil)

;; (global-unset-key (kbd "<C-return>"))
;; (global-set-key (kbd "<C-return>") 'hippie-expand)

(global-set-key (kbd "C-M-j") 'join-line)
(global-set-key (kbd "M-æ") 'hippie-expand)

(defun my-move-forward-list ()
  (interactive)
  (backward-up-list -1))

(global-set-key (kbd "C-M-æ") 'my-move-forward-list)

(setq dired-dwim-target t
      dired-recursive-copies t
      dired-listing-switches "-lha"
      dired-recursive-deletes 'top)

(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)

(setq hippie-expand-try-functions-list
      '(try-complete-file-name-partially 
       try-complete-file-name 
       try-expand-dabbrev 
       try-expand-dabbrev-all-buffers 
       try-expand-all-abbrevs 
       try-expand-list 
       try-expand-line 
       try-expand-dabbrev-from-kill 
       try-complete-lisp-symbol-partially 
       try-complete-lisp-symbol))

(global-set-key (kbd "C-S-j") 'windmove-left)
(global-set-key (kbd "C-S-i") 'windmove-up)
(global-set-key (kbd "C-S-k") 'windmove-down)
(global-set-key (kbd "C-S-l") 'windmove-right)

(defun my-nxml-mode-indent-setup ()
  (setq nxml-child-indent 4))
(add-hook 'nxml-mode-hook 'my-nxml-mode-indent-setup)

(setq exec-path (cons "~/.emacs.d/bin" exec-path))
(setq exec-path (cons "~/.emacs.d/plugins/telli" exec-path))
(setenv "PATH" (concat (getenv "HOME") "/.emacs.d/bin;"
                       (getenv "PATH")))

(global-set-key (kbd "C-x C-j") 'dired-jump)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;
;;;; cygwin support
;;;;
;; Sets your shell to use cygwin's bash, if Emacs finds it's running
;; under Windows and c:\cygwin exists. Assumes that C:\cygwin\bin is
;; not already in your Windows Path (it generally should not be).
;;
;; (let* ((cygwin-root "c:/cygwin")
(let* ((cygwin-root "c:/MinGW/msys/1.0")
       (cygwin-bin (concat cygwin-root "/bin")))
  (when (and (eq 'windows-nt system-type)
  	     (file-readable-p cygwin-root))

    (setenv "PATH" (concat cygwin-bin ";" 
                           (getenv "PATH")))

    ;; (setq exec-path (cons "e:/dev/msysgit/bin" exec-path))
    (setq exec-path (cons cygwin-bin exec-path))
    ;; (setq exec-path (cons "C:/Python27" exec-path))
    ;; (setq exec-path (append exec-path '("C:/cygwin/bin")))
    ;; (setenv "PATH" (concat "C:/Python27;" 
    ;;                        "c:/Program Files (x86)/TestDriven.NET 3/NUnit/2.5;"
    ;;                        cygwin-bin ";" 
    ;;                        "e:/dev/msysgit/bin;"
    ;;                        (getenv "PATH")
    ;;                        ";C:/cygwin/bin"
    ;;                        ))

    ;; By default use the Windows HOME.
    ;; Otherwise, uncomment below to set a HOME
    ;;      (setenv "HOME" (concat cygwin-root "/home/eric"))

    ;; NT-emacs assumes a Windows shell. Change to baash.
    (setq shell-file-name "sh")
    (setenv "SHELL" shell-file-name)
    (setq explicit-shell-file-name shell-file-name)
    ;; (setq explicit-sh-args '("--login" "-i"))
    (setq explicit-sh-args '("--noediting" "--login" "-i"))

    ;; (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
    ;; This removes unsightly ^M characters that would otherwise
    ;; appear in the output of java applications.
    (add-hook 'comint-output-filter-functions 'comint-strip-ctrl-m)))

;; (add-hook 'comint-output-filter-functions 'shell-strip-ctrl-m)

;; (add-to-list 'process-coding-system-alist
;;               '("bash" . (undecided-dos . undecided-unix)))

;; (add-to-list 'process-coding-system-alist
;;               '("sh" . (undecided-dos . undecided-unix)))

(defadvice grep-compute-defaults (around grep-compute-defaults-advice-null-device)
  "Use cygwin's /dev/null as the null-device."
  (let ((null-device "/dev/null"))
	ad-do-it))
(ad-activate 'grep-compute-defaults)
(setq null-device "/dev/null")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq-default tab-width 4)
(setq-default c-basic-offset 4)
(setq-default indent-tabs-mode nil)
(setq-default c-hungry-delete-key t)

(defun my-c++-mode-hook ()
  (c-set-style "ellemtel")
  ;; (c-set-offset 'substatement-open 0)
  (setq c-basic-offset 4)
  ;; (xgtags-mode)
  (ggtags-mode)
  ;; (highlight-symbol-mode)
  ;; (setq highlight-symbol-nav-mode t)
  (local-set-key (kbd "C-M-z") 'sp-slurp-hybrid-sexp)
  (local-set-key (kbd "RET") 'newline-and-indent)
  (local-set-key (kbd "C-c r") 'ff-find-related-file)
  (local-set-key (kbd "C-c u") 'stp-decorate-unique-ptr)
  (local-set-key (kbd "C-c s") 'stp-decorate-shared-ptr)
  (local-set-key (kbd "C-c m") 'stp-decorate-move)
  (local-set-key (kbd "C-c a") 'stp-decorate-atomic)
  (local-set-key (kbd "C-c i") 'stp-decorate-include)
  (local-set-key (kbd "C-c c") 'stp-decorate-cast))
(add-hook 'c++-mode-hook 'my-c++-mode-hook)

(defun is-c-mode-derived ()
  (let ((current-mode (buffer-local-value 'major-mode (current-buffer))))
    (or (eq 'c++-mode current-mode)
        (eq 'csharp-mode current-mode)
        (eq 'c-mode current-mode))))

(defun my-c-mode-open-brace (open-pair in-string)
  (when (and (string= open-pair "{")
             (not in-string)
             (is-c-mode-derived))
    ;; (message "found blaze")
    (c-indent-line)
    nil
    ))

(add-hook 'sp-autoinsert-inhibit-functions 'my-c-mode-open-brace)

(add-hook 'eshell-first-time-mode-hook (lambda () 
                                         (local-set-key (kbd "C-S-i") 'windmove-up)))

(setq auto-mode-alist (cons '("\\.xaml$" . nxml-mode) auto-mode-alist))
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(setq auto-mode-alist (cons '("\\.cshtml$" . html-mode) auto-mode-alist))
(add-hook 'html-mode-hook
          (lambda()
            (setq sgml-basic-offset 4)))
 
(setq auto-mode-alist
	  (append '(("\\.csproj$" . nxml-mode)) auto-mode-alist))

(setq ffap-c-path '("." "../include" "../ffmpeg/include" "C:/Program Files (x86)/Microsoft Visual Studio 11.0/VC/INCLUDE"))
(setq cc-search-directories '("." "../include" "../*"))

;; (defun ac-cc-mode-setup ()
;;   (setq ac-sources '(ac-source-params  ac-source-etags ac-source-yasnippet ac-source-words-in-same-mode-buffers )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'compile)

(defun compile-cpp ()
  (interactive)
  (let ((file (file-name-nondirectory (buffer-file-name))))
    (setq compile-command (concat "cl.exe -EHsc " file " && ./" (file-name-sans-extension file) ".exe"))
    (call-interactively 'compile)
    ))

(defun compile-sln ()
  (interactive)
  (let ((sln (car (file-expand-wildcards "*.sln"))))
  (if sln
      (setq compile-command (concat "MSBuild.exe -m -v:m " sln))
    (setq compile-command "make -k -j 2 " sln))
  )
  (call-interactively 'compile)
)

(setq compilation-ask-about-save nil)
;; (setq compilation-read-command nil)
(setq compilation-scroll-output t)
(setq compilation-window-height 20)
(setq compile-auto-highlight t)

(push '("^\\(.*\\)(\\([0-9]+\\),\\([0-9]+\\)): error" 1 2 3 2) compilation-error-regexp-alist)
(push '("^\\(.*\\)(\\([0-9]+\\),\\([0-9]+\\)): warning" 1 2 3 1) compilation-error-regexp-alist)
(push '("^\\(?:\s+[0-9]+>\\)+\\(.*\\)(\\([0-9]+\\)): error" 1 2 nil 2) compilation-error-regexp-alist)
(push '("^\\(?:\s+[0-9]+>\\)+\\(.*\\)(\\([0-9]+\\)): warning" 1 2 nil 1) compilation-error-regexp-alist)
(push '("^\\(?:\s+[0-9]+>\\)+\\(.*\\)(\\([0-9]+\\),\\([0-9]+\\)): error" 1 2 nil 2) compilation-error-regexp-alist)
(push '("^\\(?:\s+[0-9]+>\\)+\\(.*\\)(\\([0-9]+\\),\\([0-9]+\\)): warning" 1 2 nil 1) compilation-error-regexp-alist)
(push '("^\\(?:\s*\\)\\(.*\\)(\\([0-9]+\\))\s*:\s*see" 1 2 nil 2) compilation-error-regexp-alist)

(setq compilation-skip-threshold 2)
;; (setq compilation-directory-matcher '("\\(?:is building\\|Done Building Projec\\(t\\)\\) \"\\(.*\\)\\\\.*" (2 . 1)))
;; (setq compilation-page-delimiter "^\\(?:.*\\(?:is building\\|Done Building Project\\) \".*\n\\)+")
(push '("^at.*in \\(.*\\):line \\(.*\\)" 1 2 nil 2) compilation-error-regexp-alist)

(defadvice compilation-filter (around compilation-filter-around (proc string))
  ;; (setq string (concat "------- begin -------\n\n" string "\n\n--------- end --------\n\n"))
  (let ((start 0))
    (while (string-match "^\\(\s+\\(?:+[0-9]+>\\)?\\)?\\([^:]*\\(?:cs\\|cpp\\)\\)\\((.*\\[\\)\\(.*\\\\\\)\\(.*\\(?:s\\|_\\|x\\)proj\\]$\\)" string start)
    ;; (while (string-match "^\\(\s+\\(?:+[0-9]+>\\)?\\)?\\(.*\\(?:cs\\|cpp\\)\\)\\((.*\\[\\)\\(.*\\\\\\)\\(.*\\(?:s\\|_\\|x\\)proj\\]$\\)" string start)
    ;; (while (string-match "^\\(\s+\\(?:[0-9]+>\\)\\)?\\(.*\\)\\((.*\\[\\)\\(.*\\\\\\)\\(.*csproj\\]$\\)" string start)
      (setq start (match-end 5))
      (setq string
            (replace-match 
             (concat 
              (match-string-no-properties 1 string)
              (match-string-no-properties 4 string)
              (match-string-no-properties 2 string)
              (match-string-no-properties 3 string)
              (match-string-no-properties 5 string)) t t string)
            )
      )
    )
  ad-do-it
  )

(ad-activate 'compilation-filter)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (ad-deactivate 'compilation-filter)

(defun xaml-toggle ()
  (interactive)
  (let (buf-str buf-name buf-len buf-file)
	(setq buf-name (buffer-file-name))
	(setq buf-len (length buf-name))
	(if (and (> buf-len 4) (not (equal buf-name nil)))
		(progn 
		  (let (last-4)
			(setq last-4 (substring buf-name (- buf-len 4) buf-len))
			(if (string= last-4 "xaml")
				(setq buf-str (concat buf-name ".cs"))
			  (setq buf-str (substring buf-name 0 (- buf-len 3)))
			  )
			)
		  (when (file-exists-p buf-str)
			(find-file-other-window buf-str)
			)
		  )
	  )
	)
  )
(global-set-key [f7] 'xaml-toggle)

(setq ediff-diff-options "-w"
      ediff-split-window-function 'split-window-horizontally
      ediff-window-setup-function 'ediff-setup-windows-plain)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'dired)

(defun execute-dired-find-file ()
  "In dired, run the w32-shell-execute on this file."
  (interactive)
  ;; dired-get-filename is defined in dired.el
  (w32-shell-execute nil (dos-canonical-name (dired-get-filename nil t))))
(define-key dired-mode-map "\C-c\C-f" 'execute-dired-find-file)

(defun dos-canonical-name (filename)
  "Canonicalize filename forcing `\\' as directory-sep-char."
  (let ((directory-sep-char ?\ ))
    (expand-file-name filename)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun move-text-internal (arg) 
   (cond 
    ((and mark-active transient-mark-mode) 
     (if (> (point) (mark)) 
        (exchange-point-and-mark)) 
     (let ((column (current-column)) 
          (text (delete-and-extract-region (point) (mark)))) 
       (forward-line arg) 
       (move-to-column column t) 
       (set-mark (point)) 
       (insert text) 
       (exchange-point-and-mark) 
       (setq deactivate-mark nil))) 
    (t 
     (beginning-of-line) 
     (when (or (> arg 0) (not (bobp))) 
       (forward-line) 
       (when (or (< arg 0) (not (eobp))) 
        (transpose-lines arg)) 
       (forward-line -1))))) 
(defun move-text-down (arg) 
   "Move region (transient-mark-mode active) or current line 
  arg lines down." 
   (interactive "*p") 
   (move-text-internal arg)) 
(defun move-text-up (arg) 
   "Move region (transient-mark-mode active) or current line 
  arg lines up." 
   (interactive "*p") 
   (move-text-internal (- arg))) 
(global-set-key [(meta shift p)] 'move-text-up) 
(global-set-key [(meta shift n)] 'move-text-down) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun scroll-up-one-line()
  (interactive)
  (scroll-up 1))

(defun scroll-down-one-line()
  (interactive)
  (scroll-down 1))

(global-set-key [?\C-.] 'scroll-down-one-line)
(global-set-key [?\C-,] 'scroll-up-one-line)

(defun sacha/search-word-backward ()
  "Find the previous occurrence of the current word."
  (interactive)
  (push-mark)
  (let ((cur (point)))
    (skip-syntax-backward "w_")
    (goto-char
     (if (re-search-backward (concat "\\_<" (current-word) "\\_>") nil t)
         (match-beginning 0)
       cur))))

(defun sacha/search-word-forward ()
  "Find the next occurrence of the current word."
  (interactive)
  (push-mark)
  (let ((cur (point)))
    (skip-syntax-forward "w_")
    (goto-char
     (if (re-search-forward (concat "\\_<" (current-word) "\\_>") nil t)
         (match-beginning 0)
       cur))))
(global-set-key (kbd "C-S-p") 'sacha/search-word-backward)
(global-set-key (kbd "C-S-n") 'sacha/search-word-forward)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun transpose-windows ()
  (interactive)
  (let ((this-buffer (window-buffer (selected-window)))
        (other-buffer (prog2
                          (other-window +1)
                          (window-buffer (selected-window))
                        (other-window -1))))
    (switch-to-buffer other-buffer)
    (switch-to-buffer-other-window this-buffer)
    (other-window -1)))


(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun prelude-google ()
  "Googles a query or region if any."
  (interactive)
  (w3m-browse-url
   (concat
    "http://www.google.com/search?ie=utf-8&oe=utf-8&q="
    (if mark-active
        (buffer-substring (region-beginning) (region-end))
      (progn
        (let (str)
          (setq str (read-string (format "Google ('%s'): "
                                         (thing-at-point 'symbol))))
          (if (string= str "")
              (setq str (thing-at-point 'symbol))
            str)))))))


(defun google ()
  "Googles a query or region if any."
  (interactive)
  (browse-url
   (concat
    "http://www.google.com/search?ie=utf-8&oe=utf-8&q="
    (if mark-active
        (buffer-substring (region-beginning) (region-end))
      (progn
        (let (str)
          (setq str (read-string (format "Google ('%s'): "
                                         (thing-at-point 'symbol))))
          (if (string= str "")
              (setq str (thing-at-point 'symbol))
            str)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'package)
(add-to-list 'package-archives 
             '("melpa" . "http://melpa.milkbox.net/packages/"))
(package-initialize)

;; A little bit of Magnar Sveen's code
(unless (file-exists-p "~/.emacs.d/elpa/archives/melpa")
  (package-refresh-contents))

(defun packages-install (packages)
  (dolist (it packages)
    (when (not (package-installed-p it))
      (package-install it)))
  (delete-other-windows))

;; Install extensions if they're missing
(defun init--install-packages ()
  (packages-install
   '(auctex
     magit
     ;; move-text
     flx
     flx-ido
     yasnippet
     smartparens
     ido-vertical-mode
     guide-key
     wgrep
     ag
     wgrep-ag
     pos-tip
     ggtags
     smex
     ahg
     zenburn-theme
     cmake-mode
     auto-complete
     ess
     undo-tree
     projectile
     expand-region
     change-inner
     multiple-cursors
     minimap
     buffer-move
     helm
     ;; helm-swoop
     hl-line+
     ido-ubiquitous
     ;; number-font-lock-mode
     recentf-ext
     ace-jump-mode
     company
     diminish
     fold-this
)))

(condition-case nil
    (init--install-packages)
  (error
   (package-refresh-contents)
   (init--install-packages)))

(require 'change-inner)
(global-set-key (kbd "M-i") 'change-inner)
(global-set-key (kbd "M-o") 'change-outer)

(require 'undo-tree)
(global-undo-tree-mode)


(require 'hl-line+)
(defadvice switch-to-buffer (after switch-to-buffer-flash activate)
      (flash-line-highlight))
(toggle-hl-line-when-idle 1)

(require 'recentf-ext)

;; (require 'xgtags)

;; (add-hook 'xgtags-mode-hook 
;;   (lambda()
;;     (local-set-key (kbd "M-.") 'xgtags-find-tag)   ; find a tag, also M-.
;;     (local-set-key (kbd "M-,") 'xgtags-find-rtag)  ; reverse tag
;;     (local-set-key (kbd "M-*") 'xgtags-pop-stack)
;;     (local-set-key (kbd "C-M-g") 'xgtags-find-with-grep)
;;     ))

;; (require 'highlight-symbol)
;; (setq highlight-symbol-idle-delay 1.5)
;; (highlight-symbol-nav-mode)

(require 'buffer-move)
(global-set-key (kbd "M-J") 'buf-move-left)
(global-set-key (kbd "M-I") 'buf-move-up)
(global-set-key (kbd "M-K") 'buf-move-down)
(global-set-key (kbd "M-L") 'buf-move-right)

(require 'wgrep)
(setq wgrep-auto-save-buffer t)

(require 'ido)
(require 'flx-ido)
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
;; disable ido faces to see flx highlights.
(setq ido-use-faces nil)

(setq ido-vertical-define-keys nil)
(ido-vertical-mode 1)

(defun my/ido-setup-hook ()
    (define-key ido-completion-map (kbd "C-n") 'ido-next-match)
    (define-key ido-completion-map (kbd "C-p") 'ido-prev-match))

(add-hook 'ido-setup-hook 'my/ido-setup-hook)

(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-case-fold nil
      ido-auto-merge-work-directories-length -1
      ;; ido-create-new-buffer 'always
      ido-use-filename-at-point (quote guess)
      ;; ido-max-prospects 10
      ido-use-url-at-point t)

;; disable auto searching for files unless called explicitly
(setq ido-auto-merge-delay-time 99999)
(define-key ido-file-dir-completion-map (kbd "C-c C-s") 
  (lambda() 
	(interactive)
	(ido-initiate-auto-merge (current-buffer))))

(require 'guide-key)
(setq guide-key/guide-key-sequence '("C-x r" "C-x 4" "C-x 5" "C-x 8" "C-c p" "C-x v"))
(guide-key-mode 1) ; Enable guide-key-mode
(setq guide-key/popup-window-position 'bottom)

(require 'ag)

(setq ag-highlight-search nil)


(defun* ag/search (string directory
                          &key (regexp nil) (file-regex nil))
  "Run ag searching for the STRING given in DIRECTORY.
If REGEXP is non-nil, treat STRING as a regular expression."
  (let ((default-directory (file-name-as-directory directory))
        (ag-exe (if (eq 'windows-nt system-type)
                    (concat (getenv "HOME") "/.emacs.d/ag/ag.exe")
                  ag-executable))
        (arguments (if regexp
                       ag-arguments
                     (cons "--literal" ag-arguments)))
        (shell-command-switch "-c"))
    (if ag-highlight-search
        (setq arguments (append '("--color" "--color-match" "30;43") arguments))
      (setq arguments (append '("--nocolor") arguments)))
    (setq arguments (append '("--line-number") arguments))
    (when (char-or-string-p file-regex)
      (setq arguments (append `("--file-search-regex" ,file-regex) arguments)))
    (unless (file-exists-p default-directory)
      (error "No such directory %s" default-directory))
    (compilation-start
     (mapconcat 'shell-quote-argument
                (append (list ag-exe) arguments (list string))
                ;; (append '("ag") arguments (list string "."))
                " ")
     'ag-mode
     `(lambda (mode-name) ,(ag/buffer-name string directory regexp)))))


;; TEMPORARY while magit is in a limbo mode
;; (setq magit-emacsclient-executable "e:/dev/emacs-24.3/bin/emacsclient.exe")

(projectile-global-mode)
(setq projectile-indexing-method 'alien)

(defcustom my-projectile-git-tmp-command "git ls-files"
  "Command used by projectile to get the files in a git project."
  :group 'projectile
  :type 'string)

(defcustom my-projectile-hg-tmp-command "hg manifest"
  "Command used by projectile to get the files in a hg project."
  :group 'projectile
  :type 'string)

(defun my-projectile-get-tmp-command ()
  "Determine which tmp command to invoke based on the project's VCS."
  (let ((vcs (projectile-project-vcs)))
    (cond
     ((eq vcs 'git) my-projectile-git-tmp-command)
     ((eq vcs 'hg) my-projectile-hg-tmp-command)
     (t (message "Neither git nor hg project!")))))

(defun my-projectile-grep ()
  "Perform rgrep in the project."
  (interactive)
  (save-excursion
    (let ((roots (projectile-get-project-directories))
          (search-regexp (if (and transient-mark-mode mark-active)
                             (buffer-substring (region-beginning) (region-end))
                           (read-string (projectile-prepend-project-name "Grep for: ")
                                        (projectile-symbol-at-point)))))
      (dolist (root-dir roots)
        (require 'grep)
        (let ((tmp-grep (format "%stmp-grep" root-dir))
              tmp-cmd   
              (curr-dir default-directory))
          (setq tmp-cmd (format "%s > %s" (my-projectile-get-tmp-command) tmp-grep))
          (cd root-dir)
          (when (file-writable-p tmp-grep)
            (shell-command tmp-cmd)
            (shell-command (format "sed -i 's/^/\"/g' %s" tmp-grep))
            (shell-command (format "sed -i 's/$/\"/g' %s" tmp-grep))
;; "sed -i 's/ /\\\\ /g' tmp-grep"
            (grep (format "cat tmp-grep | xargs egrep -inH '%s' || true" search-regexp)))
          (cd curr-dir)
          )))))


(define-key projectile-mode-map (kbd "C-c p g") nil)
(define-key projectile-mode-map (kbd "C-c p a") nil)
(global-set-key (kbd "C-c p G") 'projectile-grep)
(global-set-key (kbd "C-c p g") 'my-projectile-grep)
(global-set-key (kbd "C-c p a") 'ag-project)
(global-set-key (kbd "C-c p A") 'ag-project-regexp)

(require 'ggtags)
(define-key ggtags-navigation-map (kbd "M-<") nil)
(define-key ggtags-navigation-map (kbd "M->") nil)
(define-key ggtags-navigation-map (kbd "M-{") nil)
(define-key ggtags-navigation-map (kbd "M-}") nil)
(define-key ggtags-navigation-map [return] nil)
(define-key ggtags-navigation-map "\r" nil)

(require 'smex) ; Not needed if you use package.el
(smex-initialize) ; Can be omitted. This might cause a (minimal) delay
                  ; when Smex is auto-initialized on its first run.
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

(require 'smartparens-config)
(sp-pair "/*" "*/")
(smartparens-global-mode t)

(sp-local-pair 'c++-mode "{" nil :post-handlers '((my-create-newline-and-enter-sexp "RET")))
 
(defun my-create-newline-and-enter-sexp (&rest _ignored)
  "Open a new brace or bracket expression, with relevant newlines and indent. "
  (newline)
  (indent-according-to-mode)
  (forward-line -1)
  (indent-according-to-mode)
)

(require 'ahg)

;; (require 'zenburn-theme)
;; (load-theme 'solarized-dark)

(require 'pos-tip)

(require 'cmake-mode)
(setq auto-mode-alist
      (append '(("CMakeLists\\.txt\\'" . cmake-mode)
                ("\\.cmake\\'" . cmake-mode))
              auto-mode-alist))

;; ;; (add-to-list 'load-path "~/.emacs.d/plugins/ac")
;; (require 'auto-complete-config)
;; ;; (add-to-list 'ac-dictionary-directories "~/.emacs.d/plugins/ac/ac-dict")
;; (ac-config-default)
;; (setq ac-auto-show-menu t)
;; ;; (setq ac-ignore-case t)
;; ;; (setq ac-ignore-case 'smart)
;; (setq ac-ignore-case t)
;; (setq ac-delay 0.05)
;; ;; (setq ac-show-menu-immediately-on-auto-complete t)
;; (define-key ac-mode-map (kbd "C-n") 'ac-next)
;; (define-key ac-mode-map (kbd "C-p") 'ac-previous)

(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
(setq company-idle-delay 0.01)
(setq company-minimum-prefix-length 1)
(setq company-transformers '(company-sort-by-occurrence))
;; (define-key company-mode-map (kbd "C-n") 'company-select-next)
;; (define-key company-mode-map (kbd "C-p") 'company-select-previous)
;; (setq company-auto-complete t)

(setq company-backends '((company-elisp company-dabbrev-code)
                         company-nxml
                         company-cmake
                         (company-keywords company-dabbrev-code company-yasnippet company-gtags)
                         company-files 
                         company-dabbrev
                         ))

(require 'yasnippet) ;; not yasnippet-bundle
;; (yas/initialize)
;; (yas/load-directory "~/.emacs.d/yasnippet-0.6.1c/snippets")
(setq yas/prompt-functions '(yas/dropdown-prompt yas/ido-prompt yas/x-prompt yas/completing-prompt yas/no-prompt))
(setf yas/indent-line 'fixed)
(yas-global-mode 1)

(global-set-key (kbd "C-M-m") 'mc/mark-next-like-this)
(global-set-key [(control meta shift m)] 'mc/unmark-next-like-this)
(global-set-key (kbd "C-M-n") 'mc/skip-to-next-like-this)
(global-set-key (kbd "C-M-,") 'mc/mark-all-symbols-like-this-in-defun)
(global-set-key (kbd "C-æ") 'er/expand-region)
(global-set-key (kbd "C-Æ") 'er/contract-region)

(defun minimap-toggle ()
  "Toggle minimap for current buffer."
  (interactive)
  (if (not (boundp 'minimap-bufname))
      (setf minimap-bufname nil))
  (if (null minimap-bufname)
      (minimap-create)
    (progn 
      (minimap-kill)
      (balance-windows))
    ))

(global-set-key [(meta shift m)] 'minimap-toggle)

(require 'ess-site)
(setq inferior-R-program-name "c:/Program Files/R/R-3.0.2/bin/i386/Rtermsetup.exe")

(require 'tex-mik)

(setq preview-gs-command "gswin32c.exe")

(setq TeX-auto-save t
	  TeX-parse-self t
	  TeX-electric-sub-and-superscript t
	  TeX-master nil
	  preview-scale-function 1.33)

(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'TeX-fold-mode)

;; (defun my-tex-hook ()
;;   (setq ac-sources (append '(ac-source-yasnippet) ac-sources)))

;; (add-hook 'LaTeX-mode-hook 'my-tex-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'load-path "~/.emacs.d/plugins")
(require 'latezen-theme)
(require 'w32-fullscreen)
(global-set-key [f11] 'w32-fullscreen)
(autoload 'dos-mode "dos" "Edit Dos scripts." t)
(add-to-list 'auto-mode-alist '("\\.bat$" . dos-mode))
(require 'skarp)
(require 'spoo)
(require 'anchored-transpose)

(global-set-key (kbd "C-x t") 'anchored-transpose)


;;; .emacs (don't put in (require 'csharp-mode))
(autoload 'csharp-mode "csharp-mode" "Major mode for editing C# code." t)
(setq csharp-want-imenu nil)

;; (load "~/.emacs.d/plugins/telli/telli.el")

;; (require 'auto-complete-etags)
(setq tags-revert-without-query t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun narrow-to-region-indirect (start end)
  "Restrict editing in this buffer to the current region, indirectly."
  (interactive "r")
  (save-excursion
	(if (region-active-p)
        (progn
          (let ((buf (clone-indirect-buffer nil nil)))
            (with-current-buffer buf
              (narrow-to-region start end))
            (switch-to-buffer buf))
		  )
	  (message "No region found!")
      )))

(global-set-key "\C-xni" 'narrow-to-region-indirect)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun stp-decorate-region (before after)
  (catch 'no-bounds
    (let (from to str out len)
      (if (region-active-p)
          (setq from (region-beginning)
                to (region-end))
        (progn
          (let (bds)
            (setq bds (bounds-of-thing-at-point 'symbol))
            (unless bds
              (throw 'no-bounds nil))
            (setq from (car bds)
                  to (cdr bds)))))
      
      (setq str (buffer-substring from to))
      (setq out (concat before str after))
      (setq len (length out))
      (save-excursion
        (delete-region from to)
        (goto-char from)
        (insert out)
        (message str))
      (forward-char len)
      len
      )))

(defun stp-decorate-ptr (before after)
  (when (stp-decorate-region before after)
      (when (looking-at "\\*")
        (delete-char 1))
    ))

(defun stp-decorate-unique-ptr ()
  (interactive)
  (stp-decorate-ptr "std::unique_ptr<" ">"))

(defun stp-decorate-shared-ptr ()
  (interactive)
  (stp-decorate-ptr "std::shared_ptr<" ">"))


(defun stp-decorate-cast ()
  (interactive)
  (let (len)
    (setq len (stp-decorate-region "static_cast<int>(" ")"))
    (when len
      (backward-char (- len 15))
      (push-mark (point))
      (backward-char 3)
      (activate-mark)
      (setq deactivate-mark nil)
      )))

(defun stp-decorate-include ()
  (interactive)
  (stp-decorate-region "#include <" ">"))

(defun stp-decorate-move (x)
  (interactive "P")
  (if x
      (stp-undecorate-region "std::move(" ")")
    (stp-decorate-region "std::move(" ")")))

(defun stp-decorate-atomic (x)
  (interactive "P")
  (if x
      (stp-undecorate-region "std::atomic<" ">")
    (stp-decorate-region "std::atomic<" ">")))

(defun stp-undecorate-region (before after)
  (let (from to str out)
    (when (region-active-p)
      (setq from (region-beginning)
            to (region-end))
      (setq str (buffer-substring from to))
      (when (string-match (concat before "\\(.*\\)" after) str)
        (setq out (match-string 1 str))
        (save-excursion
          (delete-region from to)
          (goto-char from)
          (insert out)
          (message (concat "Removed " before "..." after))
          ;; (forward-char len)
          ))
      )))

(when (eq 'windows-nt system-type)
  (setq woman-manpath '("c:/MinGW/msys/1.0/share/man" "c:/MinGW/opt/share/man"))
  )

(require 'woman)
(setq stp-man-pages
      (ignore-errors
        (woman-file-name "")
        (sort (mapcar 'car woman-topic-all-completions)
              'string-lessp)))

(defun stp-search-man-page ()
  (interactive)
  (let ((gaur (thing-at-point 'symbol)))
    (message gaur)
    (man (ido-completing-read "Man: " stp-man-pages nil nil gaur)))
    )

(global-set-key (kbd "C-c M") 'stp-search-man-page)

(require 'man)
(set-face-foreground 'Man-overstrike "#f5fffa")

(set-face-background 'hl-line "#1A1A1A")

(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)

; (add-hook 'prog-mode-hook 'number-font-lock-mode)

(defadvice kill-ring-save (before slick-copy activate compile)
  "When called interactively with no active region, copy a single
line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (message "Copied line")
     (list (line-beginning-position)
           (line-beginning-position 2)))))

(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single
  line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
           (line-beginning-position 2)))))

;; kill a line, including whitespace characters until next non-whiepsace character
;; of next line
(defadvice kill-line (before check-position activate)
  (if (member major-mode
              '(emacs-lisp-mode scheme-mode lisp-mode
                                c-mode c++-mode objc-mode
                                latex-mode plain-tex-mode))
      (if (and (eolp) (not (bolp)))
          (progn (forward-char 1)
                 (just-one-space 0)
                 (backward-char 1)))))

;; taken from prelude-editor.el
;; automatically indenting yanked text if in programming-modes
(defvar yank-indent-modes
  '(LaTeX-mode TeX-mode)
  "Modes in which to indent regions that are yanked (or yank-popped).
Only modes that don't derive from `prog-mode' should be listed here.")

(defvar yank-indent-blacklisted-modes
  '(python-mode slim-mode haml-mode)
  "Modes for which auto-indenting is suppressed.")

(defvar yank-advised-indent-threshold 1000
  "Threshold (# chars) over which indentation does not automatically occur.")

(defun yank-advised-indent-function (beg end)
  "Do indentation, as long as the region isn't too large."
  (if (<= (- end beg) yank-advised-indent-threshold)
      (indent-region beg end nil)))

(defadvice yank (after yank-indent activate)
  "If current mode is one of 'yank-indent-modes,
indent yanked text (with prefix arg don't indent)."
  (if (and (not (ad-get-arg 0))
           (not (member major-mode yank-indent-blacklisted-modes))
           (or (derived-mode-p 'prog-mode)
               (member major-mode yank-indent-modes)))
      (let ((transient-mark-mode nil))
        (yank-advised-indent-function (region-beginning) (region-end)))))

(defadvice yank-pop (after yank-pop-indent activate)
  "If current mode is one of `yank-indent-modes',
indent yanked text (with prefix arg don't indent)."
  (when (and (not (ad-get-arg 0))
             (not (member major-mode yank-indent-blacklisted-modes))
             (or (derived-mode-p 'prog-mode)
                 (member major-mode yank-indent-modes)))
    (let ((transient-mark-mode nil))
      (yank-advised-indent-function (region-beginning) (region-end)))))

;; prelude-core.el
(defun prelude-duplicate-current-line-or-region (arg)
  "Duplicates the current line or region ARG times.
If there's no region, the current line will be duplicated. However, if
there's a region, all lines that region covers will be duplicated."
  (interactive "p")
  (pcase-let* ((origin (point))
               (`(,beg . ,end) (prelude-get-positions-of-line-or-region))
               (region (buffer-substring-no-properties beg end)))
    (-dotimes arg
      (lambda (n)
        (goto-char end)
        (newline)
        (insert region)
        (setq end (point))))
    (goto-char (+ origin (* (length region) arg) arg))))

;; add duplicate line function from Prelude
;; taken from prelude-core.el
(defun prelude-get-positions-of-line-or-region ()
  "Return positions (beg . end) of the current line
or region."
  (let (beg end)
    (if (and mark-active (> (point) (mark)))
        (exchange-point-and-mark))
    (setq beg (line-beginning-position))
    (if mark-active
        (exchange-point-and-mark))
    (setq end (line-end-position))
    (cons beg end)))

(global-set-key (kbd "C-M-d") 'prelude-duplicate-current-line-or-region)

(defun smart-open-line-above ()
  "Insert an empty line above the current line.
Position the cursor at it's beginning, according to the current mode."
  (interactive)
  (move-beginning-of-line nil)
  (newline-and-indent)
  (forward-line -1)
  (indent-according-to-mode))

(defun smart-open-line ()
  "Insert an empty line after the current line.
Position the cursor at its beginning, according to the current mode."
  (interactive)
  (move-end-of-line nil)
  (newline-and-indent))

(global-set-key (kbd "C-o") 'smart-open-line-above)
(global-set-key (kbd "C-S-o") 'smart-open-line)

(require 'helm)
(require 'helm-config)

(setq
 helm-google-suggest-use-curl-p t
 helm-scroll-amount 4 ; scroll 4 lines other window using M-<next>/M-<prior>
 helm-quick-update t ; do not display invisible candidates
 helm-idle-delay 0.01 ; be idle for this many seconds, before updating in delayed sources.
 helm-input-idle-delay 0.01 ; be idle for this many seconds, before updating candidate buffer
 helm-ff-search-library-in-sexp t ; search for library in `require' and `declare-function' sexp.

 ;; you can customize helm-do-grep to execute ack-grep
 ;; helm-grep-default-command "ack-grep -Hn --smart-case --no-group --no-color %e %p %f"
 ;; helm-grep-default-recurse-command "ack-grep -H --smart-case --no-group --no-color %e %p %f"
 helm-split-window-default-side 'other ;; open helm buffer in another window
 helm-split-window-in-side-p t ;; open helm buffer inside current window, not occupy whole other window
 ;; helm-buffers-favorite-modes (append helm-buffers-favorite-modes
 ;;                                     '(picture-mode artist-mode))
 helm-candidate-number-limit 200 ; limit the number of displayed canidates
 helm-M-x-requires-pattern 0     ; show all candidates when set to 0
 helm-boring-file-regexp-list
 '("\\.git$" "\\.hg$" "\\.svn$" "\\.CVS$" "\\._darcs$" "\\.la$" "\\.o$" "\\.i$") ; do not show these files in helm buffer
 helm-ff-file-name-history-use-recentf t
 helm-move-to-line-cycle-in-source t ; move to end or beginning of source
                                        ; when reaching top or bottom of source.
 ;; ido-use-virtual-buffers t      ; Needed in helm-buffers-list
 helm-buffers-fuzzy-matching t          ; fuzzy matching buffer names when non--nil
                                        ; useful in helm-mini that lists buffers
 )

(global-set-key (kbd "M-y") 'helm-show-kill-ring)

  ;;; Save current position to mark ring
(add-hook 'helm-goto-line-before-hook 'helm-save-current-pos-to-mark-ring)

(require 'helm-swoop)
(defun stp-helm-swoop ()
  (interactive)
  (push-mark)
  (helm-swoop))
(global-set-key (kbd "C-S-s") 'stp-helm-swoop)
(global-set-key (kbd "C-M-S-s") 'helm-swoop-back-to-last-point)
(define-key isearch-mode-map (kbd "C-S-s") 'helm-swoop-from-isearch)
(setq helm-swoop-split-direction 'split-window-horizontally)

(set-face-background 'helm-selection "#2A2A2A")
(set-face-foreground 'helm-swoop-target-line-face "#888888")
(set-face-background 'helm-swoop-target-line-face "#2A2A2A")
(set-face-foreground 'helm-swoop-target-word-face "#DDDDDD")
(set-face-background 'helm-swoop-target-word-face "#555555")
(set-face-foreground 'helm-match "#DDDDDD")
(set-face-background 'helm-match "#111111")

(require 'org)
;; (require 'ox-odt)
(require 'org-latex)

(setq user-full-name "Stefán Pétursson")
(setq org-export-latex-format-toc-function (lambda (bla)))
(add-to-list 'org-export-latex-classes
  '("stp-org-article"
"\\documentclass[11pt,a4paper]{article}
\\usepackage{fontspec}
\\usepackage{graphicx} 
\\usepackage{hyperref}
\\defaultfontfeatures{Mapping=tex-text}
\\setromanfont [Ligatures={Common}, Variant=01]{Linux Libertine O}
\\usepackage{geometry}
\\geometry{a4paper, textwidth=6.5in, textheight=10in,
            marginparsep=7pt, marginparwidth=.6in}
\\pagestyle{empty}
\\title{}
      [NO-DEFAULT-PACKAGES]
      [NO-PACKAGES]"
     ("\\section{%s}" . "\\section*{%s}")
     ("\\subsection{%s}" . "\\subsection*{%s}")
     ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
     ("\\paragraph{%s}" . "\\paragraph*{%s}")
     ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(setq org-latex-to-pdf-process 
  '("xelatex -interaction nonstopmode %f"
     "xelatex -interaction nonstopmode %f")) ;; for multiple passes

;; (setq org-ditaa-jar-path "e:/dev/org-7.7/contrib/scripts/ditaa.jar")
(org-babel-do-load-languages
 'org-babel-load-languages
  '( (ditaa . t)         
     (emacs-lisp . t)   
   ))

(require 'ace-jump-mode)
(set-face-foreground 'ace-jump-face-foreground "tomato")
(global-set-key (kbd "M-A") 'ace-jump-mode)
(global-set-key (kbd "C-+") 'ace-jump-mode)

(require 'diminish)
(diminish 'isearch-mode (string 32 #x279c))
(diminish 'undo-tree-mode)
(diminish 'eldoc-mode)
(diminish 'guide-key-mode)
(diminish 'smartparens-mode)
(diminish 'yas-minor-mode)
(diminish 'abbrev-mode)
(diminish 'ggtags-mode)
(eval-after-load "rainbow-mode"
  '(diminish 'rainbow-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (require 'server)
;; (or (server-running-p)
;;     (server-start))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defun open-solution-file (sln)
;;   (interactive "fOpen solution file: ")
;;   (let (org_file)
;; 	(setq org_file (format "%s.org" sln))
;; 	(shell-command (format "sln2org %s > %s" sln org_file))
;; 	(find-file-read-only org_file)
;; 	(set-sln-buffer (current-buffer))
;; 	)
;; )

;; (defun compile-sln ()
;;   (interactive)
;;   (save-excursion
;; 	(set-buffer sln-buffer)
;; 	(print (pwd))
;; 	(compile "make -k")
;; 	(refresh-tags)
;; 	;; (refresh-all-tags)
;; 	)
;; )
;; (global-set-key [f6] 'compile-sln)

;; (defun write-sln-files ()
;;   (with-temp-buffer
;; 	(insert (get-sln-files))
;; 	(when (file-writable-p "tmp-files")
;; 	  (write-region (point-min)
;; 					(point-max)
;; 					"tmp-files"))
;; 	))

;; (defun refresh-tags()
;;   (save-excursion
;;     (set-buffer sln-buffer)
;;     (write-sln-files)
;;     ;; (shell-command "etags -L tmp-files --fields=+ianmzS --c#-kinds=ceEfgimnpst")
;;     (shell-command "etags -L tmp-files")
;;     (if (boundp 'tags-completion-table)
;; 	(progn
;; 	  (setq tags-completion-table nil)
;; 	  (tags-completion-table)))
;;     ))

;; (defvar sln-buffer nil "Zeh current solution buffer")

;; (defun set-sln-buffer (sln-buf)
;;   (interactive "bSolution buffer: ")
;;   (setq sln-buffer sln-buf)
;;   (setq tags-table-list nil)
;;   ;; (tags-reset-tags-tables)
;;   (refresh-tags)
;;   (visit-tags-table "TAGS")
;;   ;; (refresh-all-tags)
;; )
  

;; (defun get-sln-files ()
;;   (save-excursion
;;     (set-buffer sln-buffer)
;;     (with-temp-buffer
;;       (insert-buffer-substring-no-properties sln-buffer)

;;       (goto-char (point-min))
;;       (keep-lines "\\[\\[")
;;       ;; code to manipulate the string here
;;       ;; ...
;;       (goto-char (point-min))
;;       (while (re-search-forward ".*\\[\\[\\(.*\\)\\]\\[.*\\]\\].*" nil t)
;; 	(replace-match "\\1"))
;;       (buffer-string)
;;       )))

;; (defun get-sln-files-list ()
;;   (let (proj-sln)
;; 	(setq proj-sln (split-string (get-sln-files) "\n"))))

;; (defun open-project-file ()
;;   (interactive)
;;   (save-excursion
;; 	(set-buffer sln-buffer)
;; 	(find-file (ido-completing-read "Open project file: " (get-sln-files-list)))
;; 	)
;; )
;; (global-set-key "\C-co" 'open-project-file)

;; (defun find-in-sln(str)
;;   "Searches for a regex in all files belonging to this solution."
;;   (interactive "P")
;;   (save-excursion
;; 	(let ((default (thing-at-point 'symbol)))
;; 	  (setq str (read-from-minibuffer (format "Search (default '%s'): " default)))
;; 	  (if (string= str "")
;; 		  (setq str default))
;; 	  (set-buffer sln-buffer)
;; 	  (with-temp-buffer
;; 		(insert (get-sln-files))

;; 		(let ((fill-column (point-max))) 
;; 		  (fill-paragraph nil))

;; 		(when (file-writable-p "tmp-grep")
;; 		  (let ((coding-system-for-write 'unix))
;; 			(write-region (point-min)
;; 						  (point-max)
;; 						  "tmp-grep"))        
;; 		  )
;; 		;; (buffer-string) ; get result
;; 		)
;; 	  ;; (message search-files)
;; 	  (grep (format "cat tmp-grep | xargs -d '\\n' egrep -inH '%s'" str))
;; 	  )
;; 	)
;;   )
;; (global-set-key "\C-cf" 'find-in-sln)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defun search-site-url (site keyword)
;;   (concat "http://www.google.com/" 
;; 		  (format "search?q=%s+site:%s&btnI"
;; 				  (url-hexify-string keyword)
;; 				  (url-hexify-string site))))

;; (defun browse-help ()
;;    "Open a window showing the MSDN documentation for the word under the point"
;;    (interactive)   
;;    (w3m-browse-url (search-site-url  "msdn.microsoft.com/en-us" (thing-at-point 'word))))

;; ;; (defun browse-help ()
;; ;;   (interactive)
;; ;;   (browse-url (format "http://msdn.microsoft.com/library/%s" (thing-at-point 'symbol)))
;; ;; )
;; (global-set-key [f1] 'browse-help)

;; ;; (when (require 'bubble-buffer nil t)
;; ;;   (global-set-key [f10] 'bubble-buffer-next)
;; ;;   (global-set-key [(shift f10)] 'bubble-buffer-previous))
;; ;; (setq bubble-buffer-omit-regexp "\\(^ .+$\\|\\*Messages\\*\\|*compilation\\*\\|\\*.+output\\*$\\|\\*TeX Help\\*$\\|\\*vc-diff\\*\\|\\*Occur\\*\\|\\*grep\\*\\|\\*cvs-diff\\*\\)")

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (add-to-list 'load-path "~/.emacs.d/matlab/")
;; (autoload 'matlab-mode "matlab" "Matlab Editing Mode" t)
;; (add-to-list
;;  'auto-mode-alist
;;  '("\\.m$" . matlab-mode))
;; (setq matlab-indent-function t)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defvar hl-tags-start-overlay nil)
;; (make-variable-buffer-local 'hl-tags-start-overlay)

;; (defvar hl-tags-end-overlay nil)
;; (make-variable-buffer-local 'hl-tags-end-overlay)

;; (defun hl-tags-context ()
;;   (save-excursion
;;     (let ((ctx (sgml-get-context)))
;;       (and ctx
;;            (if (eq (sgml-tag-type (car ctx)) 'close)
;;                (cons (sgml-get-context) ctx)
;;              (cons ctx (progn
;;                          (sgml-skip-tag-forward 1)
;;                          (backward-char 1)
;;                          (sgml-get-context))))))))

;; (defun hl-tags-update ()
;;   (let ((ctx (hl-tags-context)))
;;     (if (null ctx)
;;         (hl-tags-hide)
;;       (hl-tags-show)
;;       (move-overlay hl-tags-end-overlay
;;                     (sgml-tag-start (caar ctx))
;;                     (sgml-tag-end (caar ctx)))
;;       (move-overlay hl-tags-start-overlay
;;                     (sgml-tag-start (cadr ctx))
;;                     (sgml-tag-end (cadr ctx))))))

;; (defun hl-tags-show ()
;;   (unless hl-tags-start-overlay
;;     (setq hl-tags-start-overlay (make-overlay 1 1)
;;           hl-tags-end-overlay (make-overlay 1 1))
;;     (overlay-put hl-tags-start-overlay 'face 'show-paren-match-face)
;;     (overlay-put hl-tags-end-overlay 'face 'show-paren-match-face)))

;; (defun hl-tags-hide ()
;;   (when hl-tags-start-overlay
;;     (delete-overlay hl-tags-start-overlay)
;;     (delete-overlay hl-tags-end-overlay)))

;; (define-minor-mode hl-tags-mode
;;   "Toggle hl-tags-mode."
;;   nil "" nil
;;   (if hl-tags-mode
;;       (add-hook 'post-command-hook 'hl-tags-update nil t)
;;     (remove-hook 'post-command-hook 'hl-tags-update t)
;;     (hl-tags-hide)))


;; (provide 'hl-tags-mode)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (autoload 'ack-same "full-ack" nil t)
;; (autoload 'ack "full-ack" nil t)
;; (autoload 'ack-find-same-file "full-ack" nil t)
;; (autoload 'ack-find-file "full-ack" nil t)

;; (add-to-list 'load-path "~/.emacs.d/w3m/")
;; (require 'w3m-load)
;; (setq w3m-use-cookies t)

;; ;; (load "~/.emacs.d/plugins/python.el")
;; ;; (require 'python)
;; ;; (require 'python-autoloads)
;; (setq python-shell-interpreter "C:\\Python27\\python.exe"
;;       python-shell-interpreter-args "-i C:\\Python27\\Scripts\\ipython-script.py"
;;       python-shell-prompt-regexp "In \\[[0-9]+\\]: "
;;       python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
;;       python-shell-completion-setup-code
;;       "from IPython.core.completerlib import module_completion"
;;       python-shell-completion-module-string-code
;;       "';'.join(module_completion('''%s'''))\n"
;;       python-shell-completion-string-code
;;       "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")

;; (setenv "PRINTER" "PDFCreator")
;; (cond ((eq system-type 'windows-nt)
;;        (setq ps-printer-name "PDFCreator")
;;        (setq ps-printer-name-option "-d")
;;        ;; (setq ps-lpr-command "/bin/lpr"))) ; if you're using the Emacs in Cygwin
;;        (setq ps-lpr-command "c:/cygwin/bin/lpr.exe"))) ; if you're using the native Windows build of Emacs


;; (defun cygwin-shell ()
;;   "Run cygwin bash in shell mode."
;;   (interactive)
;;   (let ((explicit-shell-file-name "C:/cygwin/bin/bash"))
;;     (call-interactively 'shell)))


;; (defun word-dired-find-file ()
;;   "In dired, run the MS Word on this file."
;;   (interactive)
;;   ;; dired-get-filename is defined in dired.el
;;   (w32-shell-execute nil "winword" (w32-short-file-name (dired-get-filename))))
;; (define-key dired-mode-map "\C-c\C-w" 'word-dired-find-file)

;; (defun excel-dired-find-file ()
;;   "In dired, run the MS Excel on this file."
;;   (interactive)
;;   ;; dired-get-filename is defined in dired.el
;;   (w32-shell-execute nil "c:/progra~1/micros~2/office10/excel.exe"
;; 		     (w32-short-file-name (dired-get-filename))))
;; (define-key dired-mode-map "\C-cx" 'excel-dired-find-file)

;; (defun wordpad-dired-find-file ()
;;   "In dired, run the MS wordpad on this file."
;;   (interactive)
;;   ;; dired-get-filename is defined in dired.el
;;   (w32-shell-execute nil "wordpad" (w32-short-file-name (dired-get-filename))))
;; (define-key dired-mode-map "\C-c\C-p" 'wordpad-dired-find-file)

;; (defun explorer-dired-find-file ()
;;   "In dired, run the MS Windows Explorer on this file."
;;   (interactive)
;;   ;; dired-get-filename is defined in dired.el
;;   (w32-shell-execute nil "explorer" (concat "/e," (dos-canonical-name (file-name-directory (w32-short-file-name (dired-get-filename nil t)))))))
;; (define-key dired-mode-map "\C-c\C-e" 'explorer-dired-find-file)


;; (defun gsview-dired-find-file ()
;;   "In dired, run Ghostview on this file."
;;   (interactive)
;;   ;; dired-get-filename is defined in dired.el
;;   (w32-shell-execute nil "gsview32" (dired-get-filename)))
;; (define-key dired-mode-map "\C-c\C-g" 'gsview-dired-find-file)

;; (dired-get-marked-files t current-prefix-arg)

 
