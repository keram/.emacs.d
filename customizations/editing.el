;; In Unix \n we believe
(prefer-coding-system 'utf-8-unix)

(set-default-coding-systems 'utf-8-unix)
(setq-default buffer-file-coding-system 'utf-8-unix)
(setq-default default-buffer-file-coding-system 'utf-8-unix)
(set-terminal-coding-system 'utf-8-unix)
(set-keyboard-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8-unix)
(set-selection-coding-system 'utf-8-unix)

(setq buffer-file-coding-system 'utf-8-unix)
(setq default-buffer-file-coding-system 'utf-8-unix)
(setq coding-system-for-read 'utf-8-unix)
(setq coding-system-for-write 'utf-8-unix)
(setq locale-coding-system 'utf-8-unix)

;; Remove white spaces on save
(add-hook 'before-save-hook 'whitespace-cleanup)

;;;; Highlights matching parenthesis
(show-paren-mode 1)
;;
;;;; Highlight current line
(global-hl-line-mode t)
(set-face-background hl-line-face "#2A2A2A")
(set-face-foreground 'highlight nil)

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

;; C-x C-; is default comment command
;; (global-set-key (kbd "C-c C-c") 'comment-or-uncomment-region)

;; yay rainbows!
;;(global-rainbow-delimiters-mode t)
(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'rainbow-delimiters-mode)
  (add-hook 'lisp-mode-hook #'rainbow-delimiters-mode)
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
  (setq company-dabbrev-downcase nil)
)

;; create tags file in project root,
;; find . -name "*.[ch]" | ctags -e -L -
;; https://github.com/redguardtoo/company-ctags
;; (use-package company-ctags
;;   :ensure t
;;   :after 'company-ctags-auto-setup
;; )

(use-package markdown-mode
  :defer t
  :ensure t
  :mode ("\\.text\\'" "\\.markdown\\'" "\\.md\\'")
  :config
  (custom-set-variables
   '(markdown-command "/usr/bin/pandoc")))

;; multi  line edit
(use-package multiple-cursors
  :ensure t
  :bind (("C-c m c" . mc/edit-lines))
)

;; snipets
(use-package yasnippet
  :ensure t
  :hook ((prog-mode . yas-minor-mode)
           (conf-mode . yas-minor-mode)
           (text-mode . yas-minor-mode)
           (snippet-mode . yas-minor-mode)))

(use-package yasnippet-snippets
  :ensure t
  :after (yasnippet))

;; gnuplot
(use-package gnuplot
  :commands gnuplot-mode
  :defer t
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; kill line if no region active                                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; http://emacs-fu.blogspot.co.uk/2009/11/copying-lines-without-selecting-them.html
(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
           (line-beginning-position 2)))))

;; cycle through amounts of spacing
(global-set-key (kbd "M-SPC") 'cycle-spacing)

(use-package apib-mode
  :mode "\\.apib\\'"
  :defer t
  :ensure t)

(use-package key-chord
  :defer t
  :ensure t
  :config (key-chord-mode 1))

(key-chord-define-global "q0" ")")
(key-chord-define-global "q9" "(")
(key-chord-define-global "q-" "_")
(key-chord-define-global "q8" "*")
(key-chord-define-global "q7" "&")
(key-chord-define-global "q6" "^")
(key-chord-define-global "q5" "%")
(key-chord-define-global "q;" ":")
(key-chord-define-global "q'" "@")
(key-chord-define-global "q#" "~")
(key-chord-define-global "q-" "_")
(key-chord-define-global "q=" "+")
(key-chord-define-global "j2" "\"")
(key-chord-define-global "j3" "£")
(key-chord-define-global "j4" "$")
(key-chord-define-global "j5" "%")
(key-chord-define-global "q," "<")
(key-chord-define-global "q." ">")
(key-chord-define-global "q[" "{")
(key-chord-define-global "q]" "}")

(setq key-chord-two-keys-delay 0.2)
(setq key-chord-one-key-delay 0.3)

;; https://github.com/browse-kill-ring/browse-kill-ring
;; map M-y to browse-kill-ring by adding the form (browse-kill-ring-default-keybindings) to your ~/.emacs.
;; Alternatively you can map browse-kill-ring to another key combination,
;; for example (global-set-key "\C-cy" 'browse-kill-ring).
(use-package browse-kill-ring
  :defer t
  :ensure t)

;; https://www.emacswiki.org/emacs/SaveHist
;;Save History--------------------------------
;;Save mode-line history between sessions. Very good!
(setq savehist-additional-variables    ;; Also save ...
  '(search-ring regexp-search-ring)    ;; ... searches
  )
(savehist-mode t)                      ;; do this before evaluation
;;--------------------------------------------
(setq history-delete-duplicates t)

;; emacs speak statistics
(use-package ess
  :ensure t
  :defer f
  ;; :config
  ;; (key-chord-define ess-r-mode-map "w," " <- ")
  )

;; https://github.com/pashky/restclient.el
;; https://erick.navarro.io/blog/testing-an-api-with-emacs-and-restclient/
(use-package restclient
  :ensure t
  :defer t)

;; https://erick.navarro.io/blog/minimal-setup-for-elixir-development-in-emacs/
(use-package elixir-mode
  :ensure t
  :bind (:map elixir-mode-map
              ("C-c C-f" . elixir-format)))

;; https://github.com/jacktasia/dumb-jump
(use-package dumb-jump
  :ensure t
  :init
  (setq dumb-jump-selector 'helm))


(setq skeleton-pair t)
(global-set-key "(" 'skeleton-pair-insert-maybe)
(global-set-key "[" 'skeleton-pair-insert-maybe)
(global-set-key "{" 'skeleton-pair-insert-maybe)
(global-set-key (kbd "\"") 'skeleton-pair-insert-maybe)

;; Every file must end with a newline.
(setq require-final-newline t)

;; https://github.com/browse-kill-ring/browse-kill-ring
(use-package browse-kill-ring :ensure t
  ;; :general
  ;; ("M-y"  'browse-kill-ring)
  :config
  (browse-kill-ring-default-keybindings)
  (setq browse-kill-ring-highlight-current-entry t)
  (setq browse-kill-ring-highlight-inserted-item t)
  (setq browse-kill-ring-show-preview nil)
  (setq browse-kill-ring-separator
        ".-~-.-~-.-~-.-~-.
"))

(use-package idris-mode
  :ensure t
  :config
  (key-chord-define idris-mode-map "e." "=> ")
  (key-chord-define idris-mode-map "w." "-> ")
  (key-chord-define idris-mode-map "w," "<- ")
  )

(use-package haskell-mode :ensure t)

;; https://www.masteringemacs.org/article/re-builder-interactive-regexp-builder
;; you don’t have the issue of backslash plague that haunts the default settings
(require 're-builder)
(setq reb-re-syntax 'string)


;; https://github.com/editorconfig/editorconfig-emacs
;; https://editorconfig.org/
(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

;; non-‘nil’ value, identical subsequent kills
;; yield a single kill-ring entry, without duplication.
(setq kill-do-not-save-duplicates t)

;; “read-only text”, which cannot be deleted
;; non-‘nil’ value causes not displaying an error
(setq kill-read-only-ok t)

;; useful for using fill region
(setq sentence-end-double-space nil)
