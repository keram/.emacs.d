;; Changes all yes/no questions to y/n type
(fset 'yes-or-no-p 'y-or-n-p)

;; shell scripts
(setq-default sh-basic-offset 2)
(setq-default sh-indentation 2)

;; No need for ~ files when editing
(setq create-lockfiles nil)

;; Go straight to scratch buffer on startup
(setq inhibit-startup-message t)

;; store buffers
(desktop-save-mode 1)

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(use-package which-key
  :ensure t
  :config
  (which-key-mode))


;; This one has to happen after all modes that use parens are loaded
;; More at http://www.emacswiki.org/emacs/ParEdit
(use-package paredit
  :ensure t
  :init
  (autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
  :config
  (add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
  (add-hook 'lisp-mode-hook #'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
  (add-hook 'clojure-mode-hook #'enable-paredit-mode)
  (add-hook 'ielm-mode-hook    #'enable-paredit-mode)
)

(put 'dired-find-alternate-file 'disabled nil)

;; https://emacs.stackexchange.com/questions/28/safe-way-to-enable-local-variables
(setq enable-local-variables :safe)
