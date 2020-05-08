;; Sets up exec-path-from shell
;; https://github.com/purcell/exec-path-from-shell
 (when (memq window-system '(mac ns))
   (exec-path-from-shell-initialize)
   (exec-path-from-shell-copy-envs
    '("PATH")))

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :demand t
  :config
  (progn (exec-path-from-shell-initialize)))

;; shell scripts
(setq-default sh-basic-offset 2)
(setq-default sh-indentation 2)

;; The addition of "-i" forces the bash shell into interactive mode,
;; which leads to the sourcing of ~/.bashrc.
;; https://stackoverflow.com/a/42036157
;; (setq shell-command-switch "-ic")
