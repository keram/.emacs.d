(use-package org
  :defer t
  :ensure t
  :config
  (add-hook 'org-mode-hook 'org-bullets-mode))

(use-package org-bullets
  :defer t
  :ensure t)

(use-package org-plus-contrib
  :defer t
  :ensure t)

(use-package org-present
  :defer t
  :ensure t
  :config
  (add-hook 'org-present-mode-hook
            (lambda ()
              (org-present-big)
              (org-display-inline-images)
              (global-linum-mode -1)
              (global-hl-line-mode -1)))
  (add-hook 'org-present-mode-quit-hook
            (lambda ()
              (org-present-small)
              (org-remove-inline-images)
              (global-linum-mode)
              (global-hl-line-mode 1))))

(require 'org)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (clojure . t)
   (shell . t)
   (ruby . t)))

(setq org-confirm-babel-evaluate nil
      org-src-fontify-natively t
      org-src-tab-acts-natively t)
(setq org-directory "~/docs/org")
(setq org-default-notes-file (concat org-directory "/" "refile.org"))
(setq org-log-done t)
(setq org-agenda-files
      (file-expand-wildcards (concat org-directory "/" "*.org")))
(setq org-agenda-skip-scheduled-if-done t)
(setq org-refile-targets
      '((nil :maxlevel . 1)
        (org-agenda-files :maxlevel . 1)))

;; show current and next day only in agenda view by default
(setq org-agenda-span 2)

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

(run-with-idle-timer 300 t 'jump-to-org-agenda)
