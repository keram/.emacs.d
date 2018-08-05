;; Sets up exec-path-from shell
;; https://github.com/purcell/exec-path-from-shell
;; (when (memq window-system '(mac ns))
;;   (exec-path-from-shell-initialize)
;;   (exec-path-from-shell-copy-envs
;;    '("PATH")))

(use-package exec-path-from-shell
  :ensure t)

;; shell scripts
(setq-default sh-basic-offset 2)
(setq-default sh-indentation 2)

