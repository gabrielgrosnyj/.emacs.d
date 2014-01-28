
;; (defun telli/open-paren ()
;;   (if (re-search-backward "(\\(\\(?:[a-zA-Z0-9][_a-zA-Z0-9, ]*\\)?\\)\\=" nil t)
;; 	  (progn
;; 		;; (print (concat "paren match " (match-string-no-properties 1)))
;;         ;; (popup-tip "int i, double d, List<byte[]> lb"
;;         ;;            :point (point)
;;         ;;            :around t
;;         ;;            :scroll-bar t
;;         ;;            :margin t)

;; 		(match-beginning 1)
;;         ;; (point)
;;         )
;;     ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar telli/process nil)

(defun telli/start-telli ()
  (interactive)
  (let (process)
    (setq telli/buffer "")
    (setq telli/return-type "")
    (setq telli/ac-member-list nil)
    (telli/stop-telli)
    (message "Starting *telli* ...")
    (telli/debug-trace "Starting *telli* ...")
    (setq telli/buffer "")
    (apply 'make-comint 
           "telli" 
           "~/.emacs.d/plugins/telli/telli.exe" nil '())
    (setq telli/process (get-process "telli"))
    (set-process-filter telli/process 'telli/output-filter)
  ;; (add-hook 'comint-preoutput-filter-function 'telli/output-filter)
    )
  )

;; (remove-hook 'comint-preoutput-filter-functions 'telli/output-filter)
  
(defun telli/stop-telli ()
  (interactive)
  (when (get-buffer "*telli*")
    (telli/debug-trace "Stopping *telli* ...")
    (with-current-buffer "*telli*" 
      (comint-kill-subjob)
      (erase-buffer)
      (kill-buffer)
      ;; (kill-buffer (get-buffer "*telli*"))
    )))

(defun telli/check-service ()
  (unless (get-buffer "*telli*")
    (telli/debug-trace "Need to start *telli* ...")
    (telli/start-telli)))

(defvar telli/buffer "")
(defvar telli/return-type "")
(defvar telli/ac-member-list 0)
(defvar telli/current-class nil "Used for documenting the member functions")
(defvar telli/simple-member-doc "")

(defvar telli/debug-buffer "*telli-debug*")
(defvar telli/debug t)

(defun telli/debug-trace (text)
  (when telli/debug
    (with-current-buffer (get-buffer-create telli/debug-buffer)
      (goto-char (point-max))
      (insert text)
      (insert "\n")
      ;; (goto-char (point-max))
      ;; (ignore-errors (scroll-down))
      (set-window-point (get-buffer-window (current-buffer)) 
                        (buffer-size (current-buffer)))
      )))

(defun telli/enface-text (text)
  (with-temp-buffer
    (insert text)
    
    (goto-char (point-min))
    (while (re-search-forward "^[FTMP]:\\(.*\\)" nil t)
      (put-text-property
       (match-beginning 1)
       (match-end 1)
       'face 'gnus-cite-6))
       ;; 'face 'font-lock-function-name-face))

    (goto-char (point-min))
    (while (re-search-forward "^\\s-+\\([a-zA-Z_][a-zA-Z0-9_]+\\):" nil t)
      (put-text-property
       (match-beginning 1)
       (match-end 1)
       'face '(:foreground "LightSalmon")))
       ;; 'face 'org-special-keyword))
       ;; 'face 'font-lock-type-face))

    (buffer-string)))

(defun telli/output-filter (process output)
  (setq telli/buffer (concat telli/buffer output))
  (setq output "")
  (let (end)
    (when (setq end (string-match "\n<end>" telli/buffer (- (length telli/buffer) 10)))
      (when (string-match "<error>\n" telli/buffer)
        (telli/debug-trace telli/buffer)
        ;; (message (substring telli/buffer 8 end))
        (setq telli/ac-member-list 1))
      (when (string-match "<ri>\n" telli/buffer)
        (setq telli/return-type (substring telli/buffer 5 end)))
        (telli/debug-trace (concat "returned type: " telli/return-type))
      (when (string-match "<si>\n" telli/buffer)
        (setq telli/simple-member-doc (substring telli/buffer 5 end)))
        ;; (telli/debug-trace (concat "simple query: " telli/simple-member-doc))
      (when (string-match "<qi>\n" telli/buffer)
        (telli/debug-trace "query info returned")
        (display-message-or-buffer (telli/enface-text (substring telli/buffer 5 end))))
      (when (string-match "<pi>\n" telli/buffer)
        (telli/debug-trace "parameter info returned")
        (if (featurep 'pos-tip)
            (pos-tip-show (substring telli/buffer 5 end) '("LightGoldenrod" . "#101010") (point) nil 0)
          (popup-tip (substring telli/buffer 5 end) 
                     :point (point)
                     :around t
                     :scroll-bar t
                     :margin t)))
      (when (string-match "<mi>\n" telli/buffer)
        ;; (let (cpl)
        ;;   (setq cpl (ido-completing-read "Complete: " 
        ;;                        (split-string (substring telli/buffer 5 end) "\n")))
        ;;   (when cpl
        ;;     (insert cpl)))
        (setq telli/ac-member-list (split-string (substring telli/buffer 5 end) "\n"))
        (telli/debug-trace (concat "members returned: " 
                                   (number-to-string (length telli/ac-member-list))))
        )
      (setq telli/buffer "")))
  output)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
(defun telli/quick-info (query)
  (when (get-buffer "*telli*")
    (comint-send-string telli/process (concat "qi-" query "\n"))
  ))

(defun telli/param-info (query)
  (when (get-buffer "*telli*")
    (comint-send-string telli/process (concat "pi-" query "\n"))
  ))

(defun search-class-name2 (name)
  (save-excursion
    (let (type 
          (old-case case-fold-search))
      (setq case-fold-search nil)
      (when (re-search-backward
             (concat "[ \n;(]\\([]\[a-zA-Z0-9_<>]+\\)\\s-+" 
                     name 
                     "\\s-*\\(?:[;=,)]\\|in \\)") nil t)
        ;; (thing-at-point 'symbol)
        (setq type (match-string-no-properties 1))
        (telli/debug-trace (concat "searched " type))
        (when (or (string= type "as")
                (string= type "using"))
            (setq type nil))
        )
      (setq case-fold-search old-case)
      type
      )
    ))


(defun __spin_back__ ()
  (save-excursion
    (let (thingy (ret '()))
      (while (or (looking-back "\\.\\s-*[a-zA-Z0-9_]+")
                 (and (looking-at "[a-zA-Z0-9_]") (looking-back "\\.")))
        (setq ret (cons 
                   (concat (thing-at-point 'symbol) (when thingy "[]"))
                   ret))
        (setq thingy nil)
        (search-backward ".")
        (when (re-search-backward "[]\[a-zA-Z0-9_()]")
          (forward-char))
        (when (looking-back "[]]")
          (backward-sexp)
          (setq thingy t)
          )
        (when (looking-back "[)]")
          (backward-sexp)
          )
        )
      (setq ret (cons 
                 (concat (thing-at-point 'symbol) (when thingy "[]"))
                 ret))
      ;; (thing-at-point 'symbol)
      )))

(defun telli/return-info (query)
  (when (get-buffer "*telli*")
    (let (ret)
      ;; (telli/debug-trace (concat "return type for  " query))
      (comint-send-string telli/process (concat "ri-" query "\n"))
      (accept-process-output telli/process 10)
      (setq ret telli/return-type)
      (setq telli/return-type "")
      ;; (telli/debug-trace (concat "got type " ret))
      ret)
    ))


(defun telli/get-list-object (sym)
  ;; List<object>
  ;; object[]
  (let (ret)
    (if (string-match "List<\\([]\[a-zA-Z0-9_<>]+\\)>" sym)
        (setq ret (match-string-no-properties 1 sym))
      (setq ret (telli/get-array-type sym)))
    ret))


(defun telli/get-array-type (sym)
  ;; object[]
  (let (ret)
    (if (string-match "\\([a-zA-Z0-9_]+\\)\\[\\]" sym)
        (setq ret (match-string-no-properties 1 sym)))
    ret))

(defun __test_get_o_array__ ()
  (let ((data '("List<object>" "object[]" "List<byte[]>"
                "List<List<int>>" "List<List<byte[]>>" "object")))
    (mapcar 'telli/get-list-object data)
    ))

(defun telli/return-combo (type member)
  (let (obj_t obj_m ret)
    (setq obj_t (telli/get-array-type type))
    (setq obj_m (telli/get-array-type member))
    (setq ret 
          (telli/return-info
           (if obj_t
               (concat "Array." (if obj_m obj_m member))
             (concat type "." (if obj_m obj_m member))
             ))
          )
    (if obj_m
        (telli/get-list-object ret)
      ret)
    ))

(defun telli/get-document ()
  (interactive)
  (let ((type-member (telli/type-member)))
    (telli/debug-trace (concat "finding doc for " type-member))
    (telli/quick-info type-member)
  ))

(defun telli/type-member ()
  (let ((trail (__spin_back__))
        type isobjqry member obj)
    (setq type (car trail))

    (if (setq obj (telli/get-array-type type))
        (setq isobjqry t)
      (setq obj type))

    (unless (setq type (search-class-name2 obj))
      (setq type obj))
    (when isobjqry
        (setq type (telli/get-list-object type)))

    (setq trail (cdr trail))
    (dolist (member (butlast trail))
      ;; (message (concat type " --- " member))
      (setq type (telli/return-combo type member))
      )
    (setq member (car (last trail)))
    ;; (when (string-match "\\([a-zA-Z0-9_]+\\)\\[\\]" type)
    (when (telli/get-array-type type)
      (setq type "Array"))
    (if (= (length member) 0)
        type
      (concat type "." member))
    )
  )


(defun telli/find-org-pos ()
  (save-excursion
    (let ((ret (point-at-bol)))
      (while (or (looking-back "\\.\\s-*[a-zA-Z0-9_]+")
                 (and (looking-at "[a-zA-Z0-9_]") (looking-back "\\.")))
        (search-backward ".")
        (when (re-search-backward "[]\[a-zA-Z0-9_()]")
          (forward-char))
        (when (looking-back "[]]")
          (backward-sexp))
        (setq ret (point))
        )
      ret
      )
    ))

(defun telli/get-parameters ()
  (interactive)
  (save-excursion
    (let ((org (telli/find-org-pos)))
      (unless (looking-at "(")
        (search-backward "(" org t))
      (let ((type-member (telli/type-member)))
        (telli/debug-trace (concat "finding param for " type-member))
        (telli/param-info type-member)
        ))))

(defun telli/member-info (query)
  (when (get-buffer "*telli*")
    (comint-send-string telli/process (concat "mi-" query "\n"))
    ))



(defun telli/implicit-defs ()
  (save-excursion
    (let (pt)
      (when (looking-back "[)]")
        (save-excursion
          (backward-sexp)
          (setq pt (point)))
        (when (re-search-backward
               (concat "(\\s-*"
                       "\\(?:new\\|[a-zA-Z0-9_]+\\s-+as\\)"
                       "\\s-+"
                       "\\([]\[a-zA-Z0-9_<>]+\\)"
                       "\\(?:([^)]*)\\)?)")
               pt t)
          (telli/debug-trace (concat "implicit: " (match-string-no-properties 1)))
          (match-string-no-properties 1))))))

(defun telli/member-spin-back ()
  (save-excursion
    (let (thingy 
          weirdo
          (ret '()))
      (when (looking-at "[a-zA-Z0-9_]")
        (backward-sexp))
      (when (and (looking-at "[\n ]") (looking-back "[a-zA-Z0-9_]"))
        (backward-sexp))
      (when (looking-back "\\.")
        (backward-char 1))
      (when (looking-back "[]]")
        (setq thingy t)
        (backward-sexp))
      (when (looking-back "[)]")
        (setq weirdo (telli/implicit-defs))
        (backward-sexp))
      (while (or (looking-back "\\.\\s-*[a-zA-Z0-9_]+")
                 (and (looking-at "[a-zA-Z0-9_]") (looking-back "\\.")))
        (setq weirdo nil)
        (setq ret (cons 
                   (concat (thing-at-point 'symbol) (when thingy "[]"))
                   ret))
        (setq thingy nil)
        (search-backward ".")
        (when (re-search-backward "[]\[a-zA-Z0-9_()]")
          (forward-char))
        (when (looking-back "[]]")
          (backward-sexp)
          (setq thingy t)
          )
        (when (looking-back "[)]")
          (setq weirdo (telli/implicit-defs))
          (backward-sexp)
          )
        )
      (if weirdo
          (setq ret (cons (concat weirdo (when thingy "[]")) ret))
        (setq ret (cons 
                   (concat (thing-at-point 'symbol) (when thingy "[]"))
                   ret)))
      ;; (thing-at-point 'symbol)
      )))

(defun telli/get-members ()
  (let ((trail (telli/member-spin-back))
        type isobjqry member obj)
    (setq type (car trail))
    (if (setq obj (telli/get-array-type type))
        (setq isobjqry t)
      (setq obj type))

    (unless (setq type (search-class-name2 obj))
      (setq type obj))

    (when isobjqry
      (setq type (telli/get-list-object type)))

    (when (equal type nil)
      (setq type obj))

    (setq trail (cdr trail))
    (dolist (member trail)
      (setq obj type)
      (setq type (telli/return-combo type member))
      )

    (when (telli/get-array-type type)
      (setq type "Array"))
    (when (= (length type) 0)
      (telli/debug-trace "type is empty")
      (setq type obj))
    (telli/debug-trace (concat "get members for " type))
    (setq telli/current-class type)
    (telli/member-info  type)
    )
  )

(defun telli/member-list ()
  ;; (telli/debug-trace (concat "m-list "))
  ;; 0  ;; '("foo" "bar")
  ;; )
  (if (numberp telli/ac-member-list)
      nil
    telli/ac-member-list)
  )

(defun telli/load-members ()
  (if (get-buffer "*telli*")
      (let ((ret 0)
            (cnt 500))
        ;; (message (format "wrid %d " (random 100)))
        (setq telli/ac-member-list 0)
        (telli/get-members)
        (while (and (> cnt 0)
                    (equal telli/ac-member-list 0))
          (setq cnt (1- cnt))
          (sleep-for 0.01))
        ;; (sit-for 0.01))
        (telli/debug-trace (concat "cnt " (number-to-string cnt)))
        ;; (accept-process-output telli/process 3)
        (if (listp telli/ac-member-list) 
            (telli/debug-trace (concat "returning (string) "
                                       (number-to-string (length telli/ac-member-list))))
          (telli/debug-trace (concat "returning (number) " 
                                     (number-to-string telli/ac-member-list))))
        ;; (setq ret telli/ac-member-list)
        ;; (setq telli/ac-member-list 0)
        ;; ret
        )
    0)


  ;; (let ((ret telli/ac-member-list))
  ;;   (setq telli/ac-member-list nil)
  ;;   ret)
  ;; (when (ac-menu-live-p)
    ;; (list "foo" "bar" "baz" "foobar")
    ;; )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq ac-auto-show-menu 0.5)
(setq ac-delay 0.05)
(setq ac-show-menu nil)


(defun ac-prefix-sharp-dot ()
  ;; (telli/debug-trace (concat "dot test: " ac-prefix))
  ;; )
  "C-like languages dot(.) prefix."
  ;; (if (re-search-backward "[^)]\\.\\(\\(?:[a-zA-Z0-9_][_a-zA-Z0-9]*\\)?\\)\\=" nil t)
  (if (re-search-backward "\\.\\(\\(?:[a-zA-Z_][_a-zA-Z0-9]*\\)?\\)\\=" nil t)
	  (progn
        (telli/debug-trace (concat "dot match " (match-string-no-properties 1)))
		;; (print (concat ". dot match " (match-string-no-properties 1)))
		(match-beginning 1))
    ))



(defun telli/simple-member-doc (query)
  (when (get-buffer "*telli*")
    (let (ret)
      (comint-send-string telli/process (concat "si-" query "\n"))
      (accept-process-output telli/process 10)
      (setq ret telli/simple-member-doc)
      (setq telli/simple-member-doc "")
      ret)
    ))

(defun telli/members-document (symbol)
  (telli/debug-trace (concat "Simple doc for " telli/current-class "." symbol))
  (telli/simple-member-doc (concat telli/current-class "." symbol))
  )

(defun telli/ac-init-members()
  (telli/debug-trace "init members")
  (telli/load-members)
  )
  

(defvar telli/ac-members nil)
(defvar telli/ac-etags-members nil)
;; (defvar telli/ac-etags-members-list nil)

(setq telli/ac-members
  '((init . telli/ac-init-members)
    (candidates . telli/member-list)
  ;; '((candidates . '("foo" "bar" "baz" "foobar"))
    (document . telli/members-document)
    ;; (prefix . "fruss\\(.*\\)")
    ;; (prefix . "fruss")
    ;; (prefix . c-dot)
    (prefix . ac-prefix-sharp-dot)
    (requires . 0)
    (symbol . "m")
    (cache)
	))

(defun telli/ac-etags-member-list()
  (telli/debug-trace (concat "getting etags members " ac-target))
  (all-completions ac-target (tags-completion-table))
  )

(defface ac-etags-candidate-face
  '((t (:background "#101010" :foreground "LightSalmon")))
  "Face for etags candidate")

(defface ac-etags-selection-face
  '((t (:background "black" :foreground "LightSalmon")))
  "Face for the etags selected candidate.")

(setq telli/ac-etags-members
  '(
    ;; (init . telli/ac-init-members)
    (candidates . telli/ac-etags-member-list)
  ;; '((candidates . '("foo" "bar" "baz" "foobar"))
    ;; (candidate-face . ac-etags-candidate-face)
    ;; (selection-face . ac-etags-selection-face)
    (requires . 3)
    (symbol . "e")
    (cache)
	))

(defun my-csharp-mode ()
  (csharp-mode)
  (c-set-style "c#")
  (local-set-key (kbd "C-M-a") 'csharp-move-back-to-beginning-of-defun)
  (local-set-key (kbd "C-M-e") 'csharp-move-fwd-to-end-of-defun)
  (auto-complete-mode)
  (telli/check-service)
  (setq indicate-empty-lines nil)
  (local-set-key (kbd "C-c d") 'telli/get-document)
  (local-set-key (kbd "C-c p") 'telli/get-parameters)
  (local-set-key (kbd "RET") 'newline-and-indent)
  ;; (setq ac-sources '(ac-source-members-paren ac-source-members ac-source-params ac-source-words-in-same-mode-buffers))
  ;; (setq ac-sources '(ac-source-words-in-same-mode-buffers ac-source-etags ac-source-yasnippet))
  ;; (setq ac-sources '(telli/ac-members telli/ac-etags-members ac-source-yasnippet ac-source-words-in-same-mode-buffers))
  (setq ac-sources '(telli/ac-members ac-source-yasnippet ac-source-words-in-same-mode-buffers))
  )
(setq auto-mode-alist
	  (append '(("\\.cs$" . my-csharp-mode)) auto-mode-alist))

(defun telli/simple-members ()
  (setq ac-sources '(telli/ac-members))
  )  

(defun telli/moderate-members ()
  (setq ac-sources '(telli/ac-members ac-source-yasnippet ac-source-words-in-same-mode-buffers))
  )
