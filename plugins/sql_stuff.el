(defun sql/send-message (msg)
  (save-excursion
    (set-buffer sql-buffer)
    (sql-send-string (concat "select \"" msg "\"\ngo\n"))
    ))

(defun sql/list-databases-alt()
  (interactive)
  (save-excursion
    (set-buffer sql-buffer)
    (sql/send-message "-- Database list --")
    (sql-send-string 
     (concat "select name from sys.databases\n"
             "go\n"))))

(defun sql/list-databases()
  (interactive)
  (save-excursion
    (set-buffer sql-buffer) ;; "*SQL*") 
    ;; (goto-char (point-max))
    ;; (insert-string " -- Database list --\n")
    (sql/send-message "-- Database list --")
    (sql-send-string 
     (concat "select substring(database_name, 1,30),database_size from openquery(IX, 'exec sp_databases')\n"
             "go\n"))))

(defun sql/use-database(db)
  (interactive "P")
  (let ((default (thing-at-point 'symbol)))
    (setq db (read-from-minibuffer (format "Open database (default '%s'): " default)))
    (when (string= db "")
      (setq db default))
    (sql-send-string (concat "use " db "\ngo\n"))))

(defun sql/list-tables ()
  (interactive)
  (save-excursion
    (set-buffer sql-buffer)
    ;; (goto-char (point-max))
    (sql/send-message "-- Table list --")
    ;; (goto-char (point-max))
  ;; (save-excursion
    (sql-send-string (concat 
                    "SELECT SUBSTRING(name, 1, 30) FROM sysobjects WHERE xtype = 'U' ORDER BY 1\n"
                    "go\n"))
    ))

(defun sql/list-columns(table)
  (interactive "P")
  (save-excursion
    (let ((default (thing-at-point 'symbol)))
      (setq table (read-from-minibuffer (format "Table columns (default '%s'): " default)))
      (when (string= table "")
        (setq table default))
      (set-buffer sql-buffer)
      ;; (goto-char (point-max))
      ;; (sql-send-string (concat " -- Columns for " table "--\n"))
      (sql/send-message (concat " -- Columns for " table " --\n"))
      ;; (goto-char (point-max))
      (sql-send-string 
       (concat
        ;; "SELECT SUBSTRING(name, 1, 30) FROM syscolumns WHERE id=object_id('"
        ;; table
        ;; "')\ngo\n"
        "SELECT substring(column_name,1,30) 'Column Name', substring(data_type,1,30) 'Data Type', character_maximum_length 'Maximum Length' from information_schema.columns where table_name = '"
        table
        "'\ngo\n"
        ))
      )))

(defun sql/count-rows (table)
  (interactive "P")
  (save-excursion
    (let ((default (thing-at-point 'symbol)))
      (setq table (read-from-minibuffer (format "Count rows for table (default '%s'): " default)))
      (when (string= table "")
        (setq table default))
      (set-buffer sql-buffer)
      (sql/send-message (concat " -- Rowcount for " table " --\n"))
      (sql-send-string 
       (concat
        "SELECT count(*) from ["
        table
        "]\ngo\n"
        ))
      )))

(defun my-sql-mode ()
  (toggle-truncate-lines 1)
  (local-set-key (kbd "C-c d") 'sql/list-databases)
  (local-set-key (kbd "C-c a") 'sql/list-databases-alt)
  (local-set-key (kbd "C-c t") 'sql/list-tables)
  (local-set-key (kbd "C-c c") 'sql/list-columns)
  (local-set-key (kbd "C-c n") 'sql/count-rows)
  )

(add-hook 'sql-interactive-mode-hook 'my-sql-mode)


(provide 'sql_stuff)