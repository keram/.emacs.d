;; Git config files.
;; from https://github.com/pdcawley/dotemacs
(use-package gitignore-mode :ensure t)
(use-package gitconfig-mode :ensure t)
(use-package gitattributes-mode :ensure t)

(use-package git-gutter+ :ensure t
  :diminish git-gutter+-mode
  :diminish git-gutter-mode
  :init
  ;; (bindings|add-toggle git-gutter :mode git-gutter+-mode :toggle-keys "Tg")
  (global-git-gutter+-mode 1)
  )

;; https://github.com/emacsorphanage/git-gutter-fringe
;; +- signs on left side of the code
(use-package git-gutter-fringe+ :ensure t
  :config
  (git-gutter-fr+-minimal))

;; (use-package magit
;;   :ensure t
;;   :init (require 'transient)
;;   :config
;;   ;; https://magit.vc/manual/magit/Performance.html#Performance
;;   (remove-hook 'server-switch-hook 'magit-commit-diff)
;;   (setq magit-refresh-status-buffer nil)
;;   (setq vc-handled-backends nil)
;;   :bind (("C-x C-M-g" . magit-status))
;;   )
