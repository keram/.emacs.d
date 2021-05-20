;; OSx ls does not support --dired; see ‘dired-use-ls-dired’ for more details.
;; https://stackoverflow.com/questions/25125200/emacs-error-ls-does-not-support-dired
(when (string= system-type "darwin")
  (setq dired-use-ls-dired nil))

(when (string= system-type "darwin")
  (setq markdown-command "/usr/bin/pandoc"))
