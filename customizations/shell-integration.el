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

(if (eq system-type 'windows-nt)
    (progn
      (setq exec-path (add-to-list 'exec-path "C:/Program Files/Git/bin"))
      (setenv "PATH" (concat "C:\\Program Files\\Git\\bin;" (getenv "PATH")))))

;; The addition of "-i" forces the bash shell into interactive mode,
;; which leads to the sourcing of ~/.bashrc.
;; https://stackoverflow.com/a/42036157
;; (setq shell-command-switch "-ic")

;; by default M-x eshell will open existing instance
;; of eshell in current buffer
(defun eshell-new()
  "Open a new instance of eshell."
  (interactive)
  (eshell 'N))

;; shell scripts
(setq-default sh-basic-offset 2)
(setq-default sh-indentation 2)
