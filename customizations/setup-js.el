;; javascript / html
(use-package tagedit
  :defer t
  :ensure t)

(use-package json-mode
  :defer t
  :ensure t)

(use-package js2-mode
  :defer t
  :ensure t
  :config
  (add-hook 'js-mode-hook 'subword-mode)
)

(use-package js-doc
  :defer t
  :ensure t)

(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-hook 'html-mode-hook 'subword-mode)
(setq js-indent-level 4)
(eval-after-load "sgml-mode"
  '(progn
     (require 'tagedit)
     (tagedit-add-paredit-like-keybindings)
     (add-hook 'html-mode-hook (lambda () (tagedit-mode 1)))))

(use-package web-mode
  :defer t
  :ensure t
  :mode ("\\.phtml\\'" "\\.tpl\\.php\\'" "\\.[agj]sp\\'" "\\.as[cp]x\\'" "\\.erb\\'" "\\.mustache\\'" "\\.djhtml\\'")
;;   :config
;;   (add-hook 'web-mode-hook  'my-web-mode-hook)
)
