;; for set-difference
(require 'cl)

(use-package org
  :defer t
  :ensure t
  :config
  (add-hook 'org-mode-hook 'org-bullets-mode)
  (require 'org)
  (require 'org-protocol))

(use-package org-bullets
  :defer t
  :ensure t
  :config
  (add-hook 'org-mode-hook 'org-bullets-mode))

;; Error (use-package): Cannot load org-plus-contrib
(use-package org-plus-contrib
  :defer t
  :ensure t)

(use-package ox-reveal
  :defer t
  :ensure t
  :config (require 'ox-reveal))

(use-package org-present
  :defer t
  :ensure t
  :config
  (add-hook 'org-present-mode-hook
            (lambda ()
              (org-present-big)
              (org-display-inline-images)
              ))
  (add-hook 'org-present-mode-quit-hook
            (lambda ()
              (org-present-small)
              (org-remove-inline-images)
              )))

;; https://github.com/alf/ob-restclient.el
(use-package ob-restclient
  :defer t
  :ensure t)


(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (clojure . t)
   (shell . t)
   (gnuplot . t)
   (plantuml . t)
   (js . t)
   (dot . t)
   (ditaa . t)
   (sql . t)
   (sqlite . t)
   (ruby . t)
   (calc . t)
   (restclient . t)
   (R . t)
   (haskell . t)))

(add-to-list 'exec-path "./bin/")
(setq org-babel-sqlite3-command "sqlite3")

(setq org-confirm-babel-evaluate nil
      org-src-fontify-natively t
      org-src-tab-acts-natively t
      org-babel-clojure-backend 'cider
      ;; no extra indentation for contents in src code blocks
      org-edit-src-content-indentation 0
      org-hide-macro-markers t
      org-fontify-quote-and-verse-blocks t
      org-fontify-whole-heading-line t
      org-fontify-done-headline t
      org-hide-emphasis-markers t)

(setq org-html-validation-link nil
      org-html-coding-system 'utf-8-unix
      ;; alternative 'inline-css
      org-html-htmlize-output-type 'css)

(setq org-directory "~/docs/org"
      org-default-notes-file (concat org-directory "/" "refile.org")
      org-log-done t
      org-agenda-files (mapcar (lambda (f) (concat org-directory "/" f)) '("regular.org" "gtd.org"))
      ;; org-agenda-files (file-expand-wildcards (concat org-directory "/" "*.org"))
      org-agenda-skip-deadline-if-done t
      org-agenda-skip-scheduled-if-done t
      ;; no idea what this does so commenting out temporarilty
      ;; <2019-08-13 Tue>
      ;; org-agenda-skip-timestamp-if-done t

      ;;
      ;; org-deadline-warning-days 0

      ;; show current and next day only in agenda view by default
      org-agenda-span 2

      org-agenda-tag-filter-preset '("-RADAR")

      org-use-fast-todo-selection t
      org-enforce-todo-dependencies t
      org-ellipsis "⤵")

(setq org-agenda-text-search-extra-files
      (set-difference
       (file-expand-wildcards (concat org-directory "/" "*.org"))
       org-agenda-files))

(setq my-org-files (file-expand-wildcards (concat org-directory "/" "*.org")))
(setq org-refile-targets
      (quote ((nil :maxlevel . 2)
              (my-org-files :maxlevel . 2))))

(add-hook 'after-init-hook
          (lambda ()
            (org-agenda-list 1)))

(add-hook 'kill-emacs-hook
          (lambda ()
            (if (eq system-type 'windows-nt)
                (let ((default-directory "~/tools"))
                  (shell-command-to-string "sync-org.bat"))
              )))

(setq org-ditaa-jar-path "~/tools/ditaa.jar")

(defun jump-to-org-agenda ()
  (interactive)
  (let ((buf (get-buffer "*Org Agenda*"))
        wind)
    (if buf
        (if (setq wind (get-buffer-window buf))
            (select-window wind)
          (if (called-interactively-p)
              (progn
                (select-window (display-buffer buf t t))
                (org-fit-window-to-buffer)
                ;; (org-agenda-redo)
                )
            (with-selected-window (display-buffer buf)
              (org-fit-window-to-buffer)
              ;; (org-agenda-redo)
              )))
      (call-interactively 'org-agenda-list)
      ))
  ;;(let ((buf (get-buffer "*Calendar*")))
  ;;  (unless (get-buffer-window buf)
  ;;    (org-agenda-goto-calendar)))
  )
(setq org-agenda-hide-tags-regexp "work\\|life")

(run-with-idle-timer 1800 t 'jump-to-org-agenda)
(defun mla/org-auto-exclude-function (tag)
  "Automatic task exclusion in the agenda with / RET"
  (and (cond
        ((string= tag "life")
         t)
        ((string= tag "@life")
         t))
       (concat "-" tag)))

(setq org-agenda-auto-exclude-function 'mla/org-auto-exclude-function)


(setq org-todo-keywords
      (quote ((sequence "TODO(t)"
                        "IN_PROGRESS(i)"

                        "|"
                        "DONE(d)"
                        "CANCELLED(c)"
                        "POSTPONED(p)"
                        ))))

(defun yashi/org-agenda (&optional arg)
  (interactive "P")
  (let ((org-agenda-tag-filter-preset '("-life" "-@life")))
    (org-agenda arg "a")))

(setq org-agenda-prefix-format '(
                                 (agenda  . " • ")
                                 (timeline  . "  % s")
                                 (todo  . "")
                                 (tags  . " %i %-12:c")
                                 (search . " %i %-12:c")))

(add-hook 'calendar-load-hook
          (lambda ()
            (calendar-set-date-style 'european)))

(setq calendar-week-start-day 1)

(use-package org-web-tools :ensure t)

(use-package org-ref
  :after org
  :ensure t
  :config
  (add-hook 'org-export-before-parsing-hook 'orcp-citeproc))

(use-package poly-markdown
  :ensure t)

(use-package polymode
  :ensure t
  :config
  (require 'poly-markdown))

;;https://github.com/alphapapa/org-super-agenda
;; (use-package org-super-agenda
;;   :ensure t
;;   :after (org org-agenda)
;;   :config
;;   (org-super-agenda-mode)
;;   (setq org-super-agenda-groups
;;         '((:name "Work" :tag ("work" "@work" ":@work:"))
;;           (:name "GTD" :category ("gtd"))
;;           (:name "Other" :category ("regular"))
;;           (:name "")
;;           (:discard (:category ("radar")))
;;           (:auto-category t)
;;           )))

(setq org-todo-keyword-faces
      '(("Asses" . (:foreground "LightGoldenrod2"))
        ("Trial" . (:foreground "DarkOrchid1"))
        ;; ("Trial" . (:foreground "light slate blue"))
        ("Adopt" . (:foreground "light green"))
        ("Hold" . (:foreground "light slate gray"))
        ))


;; in order to add spacing between current day and
;; following date in org agenda-view. Added \n at beginning of
;; "\n%-10s %2d %s %4d%s"
;; taken from https://github.com/jwiegley/org-mode/blob/master/lisp/org-agenda.el#L1177-L1198
(defun mla/org-agenda-format-date-aligned (date)
  "Format a DATE string for display in the daily/weekly agenda, or timeline.
This function makes sure that dates are aligned for easy reading."
  (require 'cal-iso)
  (let* ((dayname (calendar-day-name date))
         (day (cadr date))
         (day-of-week (calendar-day-of-week date))
         (month (car date))
         (monthname (calendar-month-name month))
         (year (nth 2 date))
         (iso-week (org-days-to-iso-week
                    (calendar-absolute-from-gregorian date)))
         (weekyear (cond ((and (= month 1) (>= iso-week 52))
                          (1- year))
                         ((and (= month 12) (<= iso-week 1))
                          (1+ year))
                         (t year)))
         (weekstring (if (= day-of-week 1)
                         (format " W%02d" iso-week)
                       "")))
    (format "\n%-10s %2d %s %4d%s"
            dayname day monthname year weekstring)))

(setq org-agenda-format-date 'mla/org-agenda-format-date-aligned)

(setq org-plantuml-jar-path (expand-file-name "~/tools/plantuml.jar"))
;; warning is deactivated if the task gets scheduled and you set
(setq org-agenda-skip-deadline-prewarning-if-scheduled t)

(add-to-list 'org-structure-template-alist
             '("ql" . "src sql"))

;; alternative 'inline-css
(setq org-html-htmlize-output-type 'css)

;; https://github.com/dakrone/ox-tufte
(use-package ox-tufte
  :defer t
  :ensure t)

;; open csv files in emacs org mode with C-o
(add-to-list 'org-file-apps '("\\.csv\\'" . emacs))
