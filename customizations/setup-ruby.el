;; Ruby config
(add-hook 'compilation-finish-functions
          (lambda (buf strg)
            (switch-to-buffer-other-window "*compilation*")
            (read-only-mode)
            (goto-char (point-max))
            (local-set-key (kbd "C-q")
              (lambda () (interactive) (quit-restore-window)))))

;; match cider clojure test run cmd
(add-hook 'ruby-mode-hook
          (lambda () (local-set-key (kbd "C-c C-t C-t") #'ruby-test-run)))

(add-hook 'ruby-mode-hook
  (lambda () (hs-minor-mode)))

(eval-after-load "hideshow"
  '(add-to-list 'hs-special-modes-alist
    `(ruby-mode
      ,(rx (or "def" "class" "module" "do" "{" "[")) ; Block start
      ,(rx (or "}" "]" "end"))                       ; Block end
      ,(rx (or "#" "=begin"))                        ; Comment start
      ruby-forward-sexp nil)))

(setq ruby-insert-encoding-magic-comment nil)
(setq ruby-deep-indent-paren nil)
;; (setq js-indent-level 4)

(setq-default tab-width 2)
(setq-default c-basic-offset 2)
(setq css-indent-offset 2)
(setq js-indent-level 2)
(setq web-mode-markup-indent-offset 2)
(setq web-mode-code-indent-offset 2)
(setq web-mode-css-indent-offset 2)

(use-package rbenv
  :defer t
  :ensure t)

(use-package rubocop
  :ensure t
  :defer t
  ;; :diminish rubocop-mode
  :init
  (add-hook 'ruby-mode-hook 'rubocop-mode))
