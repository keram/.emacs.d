;; In Unix \n we believe
(prefer-coding-system 'utf-8-unix)

(set-default-coding-systems 'utf-8-unix)
(setq-default buffer-file-coding-system 'utf-8-unix)
(setq-default default-buffer-file-coding-system 'utf-8-unix)
(set-terminal-coding-system 'utf-8-unix)
(set-keyboard-coding-system 'utf-8-unix)

(setq buffer-file-coding-system 'utf-8-unix)
(setq default-buffer-file-coding-system 'utf-8-unix)
(setq coding-system-for-read 'utf-8-unix)
(setq coding-system-for-write 'utf-8-unix)

;; Remove white spaces on save
(add-hook 'before-save-hook 'whitespace-cleanup)

;;;; Highlights matching parenthesis
;;(show-paren-mode 1)
;;
;;;; Highlight current line
;;(global-hl-line-mode 1)
;;
;; Don't use hard tabs
(setq-default indent-tabs-mode nil)
;;
;;;; When you visit a file, point goes to the last place where it
;;;; was when you previously visited the same file.
;;;; http://www.emacswiki.org/emacs/SavePlace
;;;; keep track of saved places in ~/.emacs.d/places
(save-place-mode 1)
(setq save-place-forget-unreadable-files nil)
(setq save-place-file (concat user-emacs-directory "places"))

;;;; Emacs can automatically create backup files. This tells Emacs to
;;;; put all backups in ~/.emacs.d/backups. More info:
;;;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Backup-Files.html
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(setq auto-save-default nil)

;; ;; comments
;; (defun toggle-comment-on-line ()
;;   "comment or uncomment current line"
;;   (interactive)
;;   (comment-or-uncomment-region (line-beginning-position) (line-end-position)))
;; (global-set-key (kbd "C-;") 'toggle-comment-on-line)

;; yay rainbows!
;;(global-rainbow-delimiters-mode t)
(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'rainbow-delimiters-mode)  (add-hook 'lisp-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'lisp-interaction-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
)

;; Inspired by
;; https://github.com/nicferrier/creole-mode/blob/e3a2b15b228c9c1df7560ec390424040d69b8bb7/creole-mode.el#L70
(defun fill-break-p ()
  "Basically just does not fill within links."
  (memq 'face (text-properties-at (point))))

(setq fill-nobreak-predicate
      (list 'fill-break-p))

; (defun no-junk-please-were-unixish ()
;   (let ((coding-str (symbol-name buffer-file-coding-system)))
;     (when (string-match "-\\(?:dos\\|mac\\)$" coding-str)
;       (set-buffer-file-coding-system 'unix))))

; (add-hook 'find-file-hooks 'no-junk-please-were-unixish)

(use-package company
  :ensure t
;;  :bind (([C-S-i] . company-complete))
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  (setq company-idle-delay 0.2)
)

(use-package magit
  :ensure t
;;  :bind (("C-x g" . magit-status))
)

(use-package markdown-mode
  :defer t
  :ensure t
  :mode ("\\.text\\'" "\\.markdown\\'" "\\.md\\'")
  :config
  (custom-set-variables
   '(markdown-command "/usr/bin/pandoc")))

; lisp editing
(use-package lsp-mode
  :defer t
  :ensure t)

(use-package lsp-ui
  :defer t
  :ensure t
  :config
  (add-hook 'lsp-mode-hook 'lsp-ui-mode)
  (add-hook 'java-mode-hook 'flycheck-mode))

(use-package company-lsp
  :ensure t
  :config
  (push 'company-lsp company-backend))

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
; end lisp

(use-package yaml-mode
  :defer t
  :ensure t)

(use-package scala-mode
  :defer t
  :ensure t)

(use-package rust-mode
  :defer t
  :ensure t)


;; multi  line edit
(use-package multiple-cursors
  :ensure t)

(use-package yasnippet
  :ensure t)

(use-package yasnippet-snippets
  :ensure t)
