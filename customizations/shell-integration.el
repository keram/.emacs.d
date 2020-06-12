;; Sets up exec-path-from shell
;; https://github.com/purcell/exec-path-from-shell
(use-package exec-path-from-shell
  :if (or (eq system-type 'gnu/linux) (memq window-system '(mac ns)))
  :ensure t
  :config
  (progn
    (exec-path-from-shell-initialize)
    (exec-path-from-shell-copy-envs '("PATH"))))

;; shell scripts
(setq-default sh-basic-offset 2)
(setq-default sh-indentation 2)

;; The addition of "-i" forces the bash shell into interactive mode,
;; which leads to the sourcing of ~/.bashrc.
;; https://stackoverflow.com/a/42036157
;; (setq shell-command-switch "-ic")
