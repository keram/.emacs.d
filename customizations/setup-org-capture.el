;; Does not work properly with emacs-snapshot ?!
;; Capture templates for: TODO tasks, Notes, habits, meetings, and org-protocol
;; (setq org-capture-templates
;;       (quote (("t" "todo" entry (file org-default-notes-file)
;;                "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
;;               ("n" "note" entry (file org-default-notes-file)
;;                "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
;;               ("j" "Journal" entry (file+datetree (concat org-directory "/" "diary.org"))
;;                "* %?\n%U\n" :clock-in t :clock-resume t)
;;               ("w" "org-protocol" entry (file org-default-notes-file)
;;                "* TODO Review %c\n%U\n" :immediate-finish t)
;;               ("m" "Meeting" entry (file org-default-notes-file)
;;                "* MEETING with %? :MEETING:\n%U" :clock-in t :clock-resume t)
;;               ("h" "Habit" entry (file org-default-notes-file)
;;                "* NEXT %?\n%U\n%a\nSCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a .+1d/3d>>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n"))))

;; Remove empty LOGBOOK drawers on clock out
(defun bh/remove-empty-drawer-on-clock-out ()
  (interactive)
  (save-excursion
    (beginning-of-line 0)
    (org-remove-empty-drawer-at "LOGBOOK" (point))))

;;(add-hook 'org-clock-out-hook 'bh/remove-empty-drawer-on-clock-out 'append)

(global-set-key (kbd "C-c o")
                (lambda () (interactive) (find-file org-default-notes-file)))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-switchb)
