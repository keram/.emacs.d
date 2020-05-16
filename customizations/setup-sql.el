;;; init-sql.el --- Support for SQL -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; Source: https://raw.githubusercontent.com/purcell/emacs.d/master/lisp/init-sql.el
;; (if (fboundp 'with-eval-after-load)
;;     (defalias 'after-load 'with-eval-after-load)
;;   (defmacro after-load (feature &rest body)
;;     "After FEATURE is loaded, evaluate BODY."
;;     (declare (indent defun))
;;     `(eval-after-load ,feature
;;        '(progn ,@body))))

;; (after-load 'sql
;;   ;; sql-mode pretty much requires your psql to be uncustomised from stock settings
;;   (push "--no-psqlrc" sql-postgres-options))

;; (defun sanityinc/fix-postgres-prompt-regexp ()
;;   "Work around https://debbugs.gnu.org/cgi/bugreport.cgi?bug=22596.
;; Fix for the above hasn't been released as of Emacs 25.2."
;;   (when (eq sql-product 'postgres)
;;     (setq-local sql-prompt-regexp "^[[:alnum:]_]*=[#>] ")
;;     (setq-local sql-prompt-cont-regexp "^[[:alnum:]_]*[-(][#>] ")))

;; (add-hook 'sql-interactive-mode-hook 'sanityinc/fix-postgres-prompt-regexp)

;; (defun sanityinc/pop-to-sqli-buffer ()
;;   "Switch to the corresponding sqli buffer."
;;   (interactive)
;;   (if (and sql-buffer (buffer-live-p sql-buffer))
;;       (progn
;;         (pop-to-buffer sql-buffer)
;;         (goto-char (point-max)))
;;     (sql-set-sqli-buffer)
;;     (when sql-buffer
;;       (sanityinc/pop-to-sqli-buffer))))

;; (setq-default sql-input-ring-file-name
;;               (expand-file-name ".sqli_history" user-emacs-directory))

;; ;; See my answer to https://emacs.stackexchange.com/questions/657/why-do-sql-mode-and-sql-interactive-mode-not-highlight-strings-the-same-way/673
;; (defun sanityinc/font-lock-everything-in-sql-interactive-mode ()
;;   (unless (eq 'oracle sql-product)
;;     (sql-product-font-lock nil nil)))
;; (add-hook 'sql-interactive-mode-hook 'sanityinc/font-lock-everything-in-sql-interactive-mode)

;; Package ideas:
;;   - PEV
;; (defun sanityinc/sql-explain-region-as-json (beg end &optional copy)
;;   "Explain the SQL between BEG and END in detailed JSON format.
;; This is suitable for pasting into tools such as
;; http://tatiyants.com/pev/.

;; When the prefix argument COPY is non-nil, do not display the
;; resulting JSON, but instead copy it to the kill ring.

;; If the region is not active, uses the current paragraph, as per
;; `sql-send-paragraph'.

;; Connection information is taken from the special sql-* variables
;; set in the current buffer, so you will usually want to start a
;; SQLi session first, or otherwise set `sql-database' etc.

;; This command currently blocks the UI, sorry."
;;   (interactive "rP")
;;   (unless (eq sql-product 'postgres)
;;     (user-error "This command is for PostgreSQL only"))
;;   (unless (use-region-p)
;;     (setq beg (save-excursion (backward-paragraph) (point))
;;           end (save-excursion (forward-paragraph) (point))))
;;   (let ((query (buffer-substring-no-properties beg end)))
;;     (with-current-buffer (if (sql-buffer-live-p sql-buffer)
;;                              sql-buffer
;;                            (current-buffer))
;;       (let* ((process-environment
;;               (append (list (concat "PGDATABASE=" sql-database)
;;                             (concat "PGHOST=" sql-server)
;;                             (concat "PGUSER=" sql-user))
;;                       process-environment))
;;              (args (list "--no-psqlrc"
;;                          "-qAt"
;;                          "-w"             ; Never prompt for password
;;                          "-E"
;;                          "-c" (concat "EXPLAIN (ANALYZE, COSTS, VERBOSE, BUFFERS, FORMAT JSON) " query ";")
;;                          ))
;;              (err-file (make-temp-file "sql-explain-json")))
;;         (with-current-buffer (get-buffer-create "*sql-explain-json*")
;;           (setq buffer-read-only nil)
;;           (delete-region (point-min) (point-max))
;;           (let ((retcode (apply 'call-process sql-postgres-program nil (list (current-buffer) err-file) nil args)))
;;             (if (zerop retcode)
;;                 (progn
;;                   (json-mode)
;;                   (read-only-mode 1)
;;                   (if copy
;;                       (progn
;;                         (kill-ring-save (buffer-substring-no-properties (point-min) (point-max)))
;;                         (message "EXPLAIN output copied to kill-ring."))
;;                     (display-buffer (current-buffer))))
;;               (with-current-buffer (get-buffer-create "*sql-explain-errors*")
;;                 (let ((inhibit-read-only t))
;;                   (insert-file-contents err-file nil nil nil t))
;;                 (display-buffer (current-buffer))
;;                 (user-error "EXPLAIN failed")))))))))


;; (after-load 'page-break-lines
;;   (push 'sql-mode page-break-lines-modes))
;; (add-hook 'sql-mode-hook 'sqlind-minor-mode)

;; Set a global keyword to use sqlup on a region
;; (global-set-key (kbd "C-c u") 'sqlup-capitalize-keywords-in-region)

(use-package sqlup-mode
  :ensure t
  :config (progn
            ;; Capitalize keywords in SQL mode
            (add-hook 'sql-mode-hook 'sqlup-mode)
            ;; Capitalize keywords in an interactive session (e.g. psql)
            (add-hook 'sql-interactive-mode-hook 'sqlup-mode)
            )
  )

(use-package sql-indent
  :ensure t
  :config (progn
            (add-hook 'sql-mode-hook 'sqlind-minor-mode)
            )
  )
