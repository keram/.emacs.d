(require 'org-datetree)
(setq org-capture-templates
      (quote (("t" "Todo" entry (file+headline "~/docs/org/gtd.org" "GTD")
               "* TODO %?\n%U\n%a\n")
              ("n" "Note" entry (file+headline org-default-notes-file "Notes")
               "* %? :NOTE:\n%U\n%a\n")
              ("c" "Code" entry (file+headline org-default-notes-file "Code")
                ;; Prompt for tag and language
                "* %?\t%^g\n#+BEGIN_SRC %^{language}\n\n#+END_SRC")
              ("d" "Diary" entry (file+datetree "~/docs/org/diary.org")
               "* %?\n%U\n")
              ("p" "org-protocol" entry (file+headline "~/docs/org/refile.org" "Notes")
               "* %a\n%U\n%:initial\n" :immediate-finish t)
              ("L" "org-protocol" entry (file+headline org-default-notes-file "Links")
               "* %a\n%U\n%:initial\n" :immediate-finish t)
              ("w" "Work related")
              ("wa" "Work Activity" entry (file+headline "~/docs/org/work.org" "Activity")
               (file "~/docs/org/tpl/org-work-activity.txt")
               :jump-to-captured t)
              ("wd" "Work Diary" entry (file+datetree "~/docs/org/work.org")
               "* %?\n%U\n")
              ("r" "Review" entry (file+datetree "~/docs/org/diary.org")
               (file "~/docs/org/tpl/org-week-review.txt")
               :jump-to-captured t :clock-in t)
  )))

;; not sure what this does so commenting out to find out
;; ;; Remove empty LOGBOOK drawers on clock out
;; (defun bh/remove-empty-drawer-on-clock-out ()
;;   (interactive)
;;   (save-excursion
;;     (beginning-of-line 0)
;;     (org-remove-empty-drawer-at "LOGBOOK" (point))))

;; (add-hook 'org-clock-out-hook 'bh/remove-empty-drawer-on-clock-out 'append)

(global-set-key (kbd "C-c o")
                (lambda () (interactive) (find-file org-default-notes-file)))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-switchb)
(setq org-protocol-default-template-key "p")
