(use-package js2-mode
  :defer t
  :ensure t
  :config
  (add-hook 'js-mode-hook 'subword-mode)
)

;; (use-package js-doc
;;   :defer t
;;   :ensure t)

(add-to-list 'interpreter-mode-alist '("node" . js2-mode))

(use-package tide
  :ensure t
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . hs-minor-mode)
         (typescript-mode . tide-setup)
         ;; (typescript-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save)
         )
  :config
  (key-chord-define typescript-mode-map "e." " => { ")
  (key-chord-define typescript-mode-map "w." "=> ")
  (setq tide-hl-identifier-idle-time 1)
  )

;; (defface tide-hl-identifier-face
;;   '(
;;     (((background  dark)) :backround "green")
;;     (((background light)) :backround "black"))
;;   "Fringe face for current position."
;;   :group 'tide)
