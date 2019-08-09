;; Changes all yes/no questions to y/n type
(fset 'yes-or-no-p 'y-or-n-p)

;; shell scripts
(setq-default sh-basic-offset 2)
(setq-default sh-indentation 2)

;; No need for ~ files when editing
(setq create-lockfiles nil)

;; Go straight to scratch buffer on startup
(setq inhibit-startup-message t)

;; store buffers
(desktop-save-mode 1)

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(use-package which-key
  :ensure t
  :config
  (which-key-mode))


;; This one has to happen after all modes that use parens are loaded
;; More at http://www.emacswiki.org/emacs/ParEdit
(use-package paredit
  :ensure t
  :init
  (autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
  :config
  (add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
  (add-hook 'lisp-mode-hook #'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
  (add-hook 'clojure-mode-hook #'enable-paredit-mode)
  (add-hook 'ielm-mode-hook    #'enable-paredit-mode)
)

(put 'dired-find-alternate-file 'disabled nil)

;; https://emacs.stackexchange.com/questions/28/safe-way-to-enable-local-variables
(setq enable-local-variables :safe)
(setq safe-local-variable-values '((encoding . utf-8)))
(if (string-equal system-type "gnu/linux") ; Linux
 (progn
   (setq desktop-base-file-name ".emacs.linux.desktop")
   )
 )

;; http://www.coli.uni-saarland.de/~slemaguer/emacs/main.html
(defun package-upgrade-all ()
  "Upgrade all packages automatically without showing *Packages* buffer."
  (interactive)
  (package-refresh-contents)
  (let (upgrades)
    (cl-flet ((get-version (name where)
                (let ((pkg (cadr (assq name where))))
                  (when pkg
                    (package-desc-version pkg)))))
      (dolist (package (mapcar #'car package-alist))
        (let ((in-archive (get-version package package-archive-contents)))
          (when (and in-archive
                     (version-list-< (get-version package package-alist)
                                     in-archive))
            (push (cadr (assq package package-archive-contents))
                  upgrades)))))
    (if upgrades
        (when (yes-or-no-p
               (message "Upgrade %d package%s (%s)? "
                        (length upgrades)
                        (if (= (length upgrades) 1) "" "s")
                        (mapconcat #'package-desc-full-name upgrades ", ")))
          (save-window-excursion
            (dolist (package-desc upgrades)
              (let ((old-package (cadr (assq (package-desc-name package-desc)
                                             package-alist))))
                (package-install package-desc)
                (package-delete  old-package)))))
      (message "All packages are up to date"))))

;; (setq ispell-program-name "hunspell")
;; (add-to-list 'exec-path "C:/Program Files (x86)/Aspell/bin/")

;; https://github.com/nashamri/academic-phrases
;; (use-package academic-phrases :ensure t)

;; https://github.com/SavchenkoValeriy/emacs-powerthesaurus
(use-package powerthesaurus :ensure t)
(use-package config-general-mode
  :ensure t
  :mode ("\\.conf$" "\\.*rc$"))



;; https://github.com/doublep/logview
(use-package logview
  :ensure t
  :mode ("syslog\\(?:\\.[0-9]+\\)" "\\.log\\(?:\\.[0-9]+\\)?\\'"))
