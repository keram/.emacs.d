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
   (ruby . t)))

(setq org-confirm-babel-evaluate nil
      org-src-fontify-natively t
      org-src-tab-acts-natively t
      org-babel-clojure-backend 'cider
      ;; no extra indentation for contents in src code blocks
      org-edit-src-content-indentation 0)


(setq org-directory "~/docs/org"
      org-default-notes-file (concat org-directory "/" "refile.org")
      org-log-done t
      org-agenda-files (file-expand-wildcards (concat org-directory "/" "*.org"))
      org-agenda-skip-deadline-if-done t
      org-agenda-skip-scheduled-if-done t
      org-agenda-skip-timestamp-if-done t
      org-deadline-warning-days 0
      ;; show current and next day only in agenda view by default
      org-agenda-span 2
      org-use-fast-todo-selection t
      org-enforce-todo-dependencies t
      org-ellipsis "⤵")

(setq org-ditaa-jar-path "~/tools/ditaa.jar")
(setq org-refile-targets
      (quote ((nil :maxlevel . 2)
              (org-agenda-files :maxlevel . 2))))

(add-hook 'after-init-hook
          (lambda ()
            (org-agenda-list 1)))

(add-hook 'kill-emacs-hook
          (lambda ()
            (if (eq system-type 'windows-nt)
                (let ((default-directory "~/tools"))
                  (shell-command-to-string "sync-org.bat"))
              )))

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

(run-with-idle-timer 900 t 'jump-to-org-agenda)
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
;; (use-package org-plus-contrib)
;; ((agenda . " %i %-12:c%?-12t% s")
;;  (todo . " %i %-12:c")
;;  (tags . " %i %-12:c")
;;  (search . " %i %-12:c"))

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
(use-package org-super-agenda
  :ensure t
  :after (org org-agenda)
  :config
  (org-super-agenda-mode)
  (setq org-super-agenda-groups
        '((:name "Work" :tag ("work" "@work" ":@work:"))
          (:name "GTD" :category ("gtd"))
          (:name "Other" :category ("regular"))
          (:name "")
          (:auto-category t)
          )))

(setq org-todo-keyword-faces
      '(("Asses" . (:foreground "DarkOrange1"))
        ("Trial" . (:foreground "sea green"))
        ("Adopt" . (:foreground "light sea green"))
        ("Hold" . (:foreground "forest green"))
        ))
