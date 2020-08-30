(require 'package)

(setq package-archives
      (quote
       (("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("org" . "https://orgmode.org/elpa/")
        ("marmalade" . "https://marmalade-repo.org/packages/")
        ("melpa" . "https://melpa.org/packages/"))))

(when (version< emacs-version "27.0") (package-initialize))

;; this does not work for emacs 26
(unless package--initialized (package-initialize t))

;; less refresh the packages
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))
;;;;
;; Customization
;;;;

;; custom file contains settings edited while running emacs
;; content of the file is added/edited only by emacs only!!
;; do not edit/add there anything by hand
(setq custom-file (expand-file-name "system-custom.el" user-emacs-directory))

;; Add a directory to our load path so that when you `load` things
;; below, Emacs knows where to look for the corresponding file.
(add-to-list 'load-path (expand-file-name "customizations" user-emacs-directory))

;; Sets up exec-path-from-shell so that Emacs will use the correct
;; environment variables
(load "shell-integration.el")

;; These customizations make it easier for you to navigate files,
;; switch buffers, and choose options from the minibuffer.
(load "navigation.el")

;; These customizations change the way emacs looks and disable/enable
;; some user interface elements
(load "ui.el")

;; These customizations make editing a bit nicer.
(load "editing.el")

;; For editing lisps
(load "setup-lisp.el")

;; Langauage-specific
(load "setup-clojure.el")
(load "setup-js.el")
(load "setup-org.el")
(load "setup-org-capture.el")
(load "setup-ruby.el")
;; unused
;; (load "setup-go.el")
(load "setup-sql.el")
(load "setup-git.el")

;; Hard-to-categorize customizations
(load "misc.el")
;; ## added by OPAM user-setup for emacs / base ## 56ab50dc8996d2bb95e7856a6eddb17b ## you can edit, but keep this line
(require 'opam-user-setup "~/.emacs.d/opam-user-setup.el")
;; ## end of OPAM user-setup addition for emacs / base ## keep this line
