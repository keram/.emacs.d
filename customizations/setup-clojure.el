;;;;
;; Clojure
;;;;
(defun clj-clojure-setup ()
  "Functionality to be added for Clojure."
  (clj-refactor-mode 1)
  (yas-minor-mode 1)
  (show-paren-mode 1)
;;  (cljr-add-keybindings-with-prefix "C-c C-m")
)

(use-package cider
  :defer t
  :ensure t
  ;; :pin melpa-stable
  :config
  (add-hook 'cider-repl-mode-hook 'paredit-mode)
  (add-hook 'cider-mode-hook 'paredit-mode)
  (add-hook 'cider-mode-hook #'eldoc-mode)
  (add-hook 'cider-mode-hook #'company-mode)
  (add-hook 'cider-repl-mode-hook #'company-mode))

(use-package clojure-mode
  :defer t
  :ensure t
  ;; :pin melpa-stable
  :config
  (add-hook 'clojure-mode-hook 'subword-mode)
  (add-hook 'clojure-mode-hook
            (lambda ()
              (setq inferior-lisp-program "lein repl")
              (font-lock-add-keywords
               nil
               '(("(\\(facts?\\)"
                  (1 font-lock-keyword-face))
                 ("(\\(background?\\)"
                  (1 font-lock-keyword-face))))
              (define-clojure-indent
                (fact 1)
                (facts 1)
                (defroutes 'defun)
                (GET 2)
                (POST 2)
                (PUT 2)
                (DELETE 2)
                (HEAD 2)
                (ANY 2)
                (OPTIONS 2)
                (PATCH 2)
                (rfn 2)
                (let-routes 1)
                (context 2))
              ))
  (add-hook 'clojure-mode-hook #'cider-mode)
  )

;; Not sure if I use this so will try live without and see 26.12.2020
;; https://github.com/clojure-emacs/clj-refactor.el
;; (use-package clj-refactor
;;   :defer t
;;   :ensure t
;; ;  :pin melpa-stable
;;   :config
;;   (add-hook 'clojure-mode-hook #'clj-clojure-setup))

(use-package clojure-mode-extra-font-locking
  :defer t
  :ensure t
  ;; :pin melpa-stable
  )


;; https://github.com/weavejester/compojure/wiki/Emacs-indentation
;; (define-clojure-indent
;;   (defroutes 'defun)
;;   (GET 2)
;;   (POST 2)
;;   (PUT 2)
;;   (DELETE 2)
;;   (HEAD 2)
;;   (ANY 2)
;;   (OPTIONS 2)
;;   (PATCH 2)
;;   (rfn 2)
;;   (let-routes 1)
;;   (context 2))
