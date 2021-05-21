(require 'package)

(setq package-archives
      (quote
       (("gnu" . "https://elpa.gnu.org/packages/")
        ;; ("melpa-stable" . "https://stable.melpa.org/packages/")
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

;; To allow have slightly different settings
;; depending on what computer or env I'm right now
;; osx - work, win, gnu, bash on win ...
;; => windows-nt_mla_system-custom.el
(setq system-custom-filename
      (concat
       (replace-regexp-in-string "[^a-z]+" "-" (symbol-name system-type))
       "_"
       (replace-regexp-in-string "[^a-z]+" "-" system-name)
       "_"
       "system-custom.el"))

;; custom file contains settings edited while running emacs
;; content of the file is added/edited only by emacs only!!
;; do not edit/add there anything by hand
(setq custom-file (expand-file-name system-custom-filename user-emacs-directory))

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
(load "setup-sql.el")
(load "setup-git.el")

;; Hard-to-categorize customizations
(load "misc.el")

(load "modes.el")

(if (file-exists-p "local-sensitive.el")
    (load "local-sensitive.el"))

;; load system specific configuration if it exists
(let ((file-name
       (concat (replace-regexp-in-string "[^a-z]+" "-" (symbol-name system-type)) ".el")))
  (when (file-exists-p (expand-file-name
                        file-name
                        "customizations"
                        ))
    (load file-name)))
