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
  (setq company-dabbrev-downcase nil)
)

;; create tags file in project root,
;; find . -name "*.[ch]" | ctags -e -L -
;; https://github.com/redguardtoo/company-ctags
(use-package company-ctags
  :ensure t
  :after 'company-ctags-auto-setup
)

;; (eval-after-load 'company
;;   '(progn
;;      ))

(use-package magit
  :ensure t
  :init (require 'transient)
  :config
  ;; https://magit.vc/manual/magit/Performance.html#Performance
  (remove-hook 'server-switch-hook 'magit-commit-diff)
  (setq magit-refresh-status-buffer nil)
  (setq vc-handled-backends nil)
  :bind (("C-x C-M-g" . magit-status))
  )

(if (eq system-type 'windows-nt)
    (progn
      (setq exec-path (add-to-list 'exec-path "C:/Program Files/Git/bin"))
      (setenv "PATH" (concat "C:\\Program Files\\Git\\bin;" (getenv "PATH")))))

;; popup commit message at current line
;; https://github.com/syohex/emacs-git-messenger
(use-package git-messenger
  :defer t
  :ensure t)

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
  :defer t
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

;; yaml
(use-package yaml-mode
  :defer t
  :mode ("\\.yaml\\'" "\\.yml\\'")
  :ensure t)

;; scala
(use-package ensime
  :ensure t
  :defer t
  :commands (ensime-scala-mode-hook))

(use-package scala-mode
  :ensure t
  :defer t
  :mode ("\\.scala\\'" . scala-mode)
  :config (add-hook 'scala-mode-hook 'ensime-scala-mode-hook))

;; rust
(use-package rust-mode
  :defer t
  :mode "\\.rs\\'"
  :ensure t)


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

;; ocaml
(use-package tuareg
  :ensure t
  :defer t
  :mode ("\\.ml\\'" . tuareg-mode))

;; reasonml
; (use-package reason-mode
;   :ensure t
;   :defer nil)


(defun shell-cmd (cmd)
  "Returns the stdout output of a shell command or nil if the command returned
     an error"
  (car (ignore-errors (apply 'process-lines (split-string cmd)))))

(setq opam-p (shell-cmd "which opam"))
(setq reason-p (shell-cmd "which refmt"))

(if opam-p
    (let ((opam-share (ignore-errors (car (process-lines "opam" "config" "var" "share")))))
      (when (and opam-share (file-directory-p opam-share))
        (add-to-list 'load-path (expand-file-name "emacs/site-lisp" opam-share)))))

; (let ((opam-share (ignore-errors (car (process-lines "opam" "config" "var" "share")))))
;       (when (and opam-share (file-directory-p opam-share))
;        ;; Register Merlin
;        (add-to-list 'load-path (expand-file-name "emacs/site-lisp" opam-share))
;        (autoload 'merlin-mode "merlin" nil t nil)
;        ;; Automatically start it in OCaml buffers
;        (add-hook 'tuareg-mode-hook 'merlin-mode t)
;        (add-hook 'caml-mode-hook 'merlin-mode t)
;        ;; Use opam switch to lookup ocamlmerlin binary
;        (setq merlin-command 'opam)))

(use-package reason-mode
  :if reason-p
  :ensure t
  :config
  (let* ((refmt-bin (or (shell-cmd "refmt ----where")
                        (shell-cmd "which refmt")))
         (merlin-bin (or (shell-cmd "ocamlmerlin ----where")
                         (shell-cmd "which ocamlmerlin")))
         (merlin-base-dir (when merlin-bin
                            (replace-regexp-in-string "bin/ocamlmerlin$" "" merlin-bin))))
    ;; Add npm merlin.el to the emacs load path and tell emacs where to find ocamlmerlin
    (when merlin-bin
      (add-to-list 'load-path (concat merlin-base-dir "share/emacs/site-lisp/"))
      (setq merlin-command merlin-bin))
    (when refmt-bin
      (setq refmt-command refmt-bin)))
  )

(use-package ocp-indent)
;(use-package ocp-index)

(use-package merlin
  :custom
  (merlin-completion-with-doc t)
  :bind (:map merlin-mode-map
              ("M-." . merlin-locate)
              ("M-," . merlin-pop-stack)
              ("M-?" . merlin-occurrences)
              ("C-c C-j" . merlin-jump)
              ("C-c i" . merlin-locate-ident)
              ("C-c C-e" . merlin-iedit-occurrences)
              )
  :hook
  ;; Start merlin on ml files
  ((reason-mode tuareg-mode caml-mode) . merlin-mode)
  )

(use-package utop
  :custom
  (utop-edit-command nil)
  :hook
  (tuareg-mode . (lambda ()
                   (setq utop-command "utop -emacs")
                   (utop-minor-mode)))
  (reason-mode . (lambda ()
                   (setq utop-command "rtop -emacs")
                   (setq utop-prompt
                         (lambda ()
                           (let ((prompt (format "rtop[%d]> " utop-command-number)))
                             (add-text-properties 0 (length prompt) '(face utop-prompt) prompt)
                             prompt)))
                   (utop-minor-mode)))
  )

;; json
(use-package json-mode
  :mode "\\.json\\'"
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

(when (string= system-type "darwin")
  (setq markdown-command "/usr/bin/pandoc"))

(use-package key-chord
  :defer t
  :ensure t)

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

(setq key-chord-two-keys-delay 0.3)

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
  :defer t)

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

;; Git config files.
;; from https://github.com/pdcawley/dotemacs
(use-package gitignore-mode :ensure t)
(use-package gitconfig-mode :ensure t)
(use-package gitattributes-mode :ensure t)

(use-package git-gutter+ :ensure t
  :diminish git-gutter+-mode
  :diminish git-gutter-mode
  :init
  ;; (bindings|add-toggle git-gutter :mode git-gutter+-mode :toggle-keys "Tg")
  (global-git-gutter+-mode 1)
  )

(use-package git-gutter-fringe+ :ensure t
  :config
  (git-gutter-fr+-minimal))

(use-package idris-mode :ensure t)
