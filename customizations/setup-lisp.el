;; eldoc-mode shows documentation in the minibuffer when writing code
;; http://www.emacswiki.org/emacs/ElDoc
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)

;; Pretty print for lisp
(use-package ipretty :ensure t)

;; lisp editing
;; (use-package lsp-mode
;;   :defer t
;;   :ensure t)

;; (use-package lsp-ui
;;   :defer t
;;   :ensure t
;;   :config
;;   (add-hook 'lsp-mode-hook 'lsp-ui-mode)
;;   (add-hook 'java-mode-hook 'flycheck-mode))

;; (use-package company-lsp
;;   :ensure t
;;   :defer t
;;   :config
;;   (push 'company-lsp company-backend))

(use-package slime
  :defer t
  :ensure t
  :config
  (setq inferior-lisp-program "/usr/bin/sbcl")
  (setq slime-contribs '(slime-fancy)))

(use-package slime-company
  :defer t
  :ensure t
  :config
  (slime-setup '(slime-fancy slime-company)))

; scheme
(use-package geiser
  :defer t
  :ensure t)
; end lisp
