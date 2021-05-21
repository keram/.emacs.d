(use-package markdown-mode
  :defer t
  :ensure t
  :mode ("\\.text\\'" "\\.markdown\\'" "\\.md\\'")
  :config
  (custom-set-variables
   '(markdown-command "/usr/bin/pandoc")))

;; gnuplot
(use-package gnuplot
  :commands gnuplot-mode
  :defer t
  :ensure t)


(use-package apib-mode
  :mode "\\.apib\\'"
  :defer t
  :ensure t)


;; https://erick.navarro.io/blog/minimal-setup-for-elixir-development-in-emacs/
(use-package elixir-mode
  :ensure t
  :bind (:map elixir-mode-map
              ("C-c C-f" . elixir-format)))


(use-package idris-mode
  :ensure t
  :config
  (key-chord-define idris-mode-map "e." "=> ")
  (key-chord-define idris-mode-map "w." "-> ")
  (key-chord-define idris-mode-map "w," "<- ")
  )

(use-package haskell-mode :ensure t)
