;; These customizations change the way emacs looks and disable/enable
;; some user interface elements. Some useful customizations are
;; commented out, and begin with the line "CUSTOMIZE". These are more
;; a matter of preference and may require some fiddling to match your
;; preferences

;; Turn off the menu bar at the top of each frame because it's distracting
(menu-bar-mode -1)
;; icons ms
(tool-bar-mode -1)

;; Show line numbers
;; (global-display-line-numbers-mode)
;; (add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; Don't show native OS scroll bars for buffers because they're redundant
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

;; ;; Color Themes
;; ;; Read http://batsov.com/articles/2012/02/19/color-theming-in-emacs-reloaded/
;; ;; for a great explanation of emacs color themes.
;; ;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Custom-Themes.html
;; ;; for a more technical explanation.
;; (load-theme 'tomorrow-night-bright t)
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(add-to-list 'load-path "~/.emacs.d/themes")

(load-theme 'tango-dark t)
;; the amount of fontification applied by Font Lock mode,
;; for major modes that support this feature.
;; t, meaning “as high as possible” (the default).
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Font-Lock.html
(setq font-lock-maximum-decoration t)
;; (use-package solarized-theme
;;   :ensure t)
;; (load-theme 'solarized-dark t)

;; increase font size for better readability
;; (set-face-attribute 'default nil :height 140)

;; Uncomment the lines below by removing semicolons and play with the
;; values in order to set the width (in characters wide) and height
;; (in lines high) Emacs will have whenever you start it
;; (setq initial-frame-alist '((top . 0) (left . 0) (width . 177) (height . 53)))

;; These settings relate to how emacs interacts with your operating system
(setq ;; makes killing/yanking interact with the clipboard
      x-select-enable-clipboard t

      ;; I'm actually not sure what this does but it's recommended?
      x-select-enable-primary t

      ;; Save clipboard strings into kill ring before replacing them.
      ;; When one selects something in another program to paste it into Emacs,
      ;; but kills something in Emacs before actually pasting it,
      ;; this selection is gone unless this variable is non-nil
      save-interprogram-paste-before-kill t

      ;; Shows all options when running apropos. For more info,
      ;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Apropos.html
      apropos-do-all t

      ;; Mouse yank commands yank at point instead of at click.
      mouse-yank-at-point t)

;; No cursor blinking, it's distracting
(blink-cursor-mode 0)

;; full path in title bar
(setq-default frame-title-format "%b (%f)")

;; don't pop up font menu
(global-set-key (kbd "s-t") '(lambda () (interactive)))

;; no bell
(setq ring-bell-function 'ignore)

;; let's pretify those lambdas
(defun my-pretty-lambda (lambda-string)
  "Make some word or string show as pretty Unicode symbols.  LAMBDA-STRING is the way that the language declares lambda functions."
  (setq prettify-symbols-alist
        ;; λ
        '((lambda-string . 955))))

(defun my-pretty-lambda-clojure ()
  "Make some word or string show as pretty Unicode symbols.  LAMBDA-STRING is the way that the language declares lambda functions."
  (setq prettify-symbols-alist
        ;; λ
        '(("fn" . 955))))

(global-prettify-symbols-mode 1)
;; http://endlessparentheses.com/new-in-emacs-25-1-have-prettify-symbols-mode-reveal-the-symbol-at-point.html
(setq prettify-symbols-unprettify-at-point 'right-edge)

;; tmux like zoom
;; https://github.com/syohex/emacs-zoom-window
(use-package zoom-window
  :ensure t
  :bind ("C-x C-z" . zoom-window-zoom)
  :config (custom-set-variables
           '(zoom-window-mode-line-color "Black")))

(defun set-window-width (n)
  "Set the selected window's width."
   (adjust-window-trailing-edge (selected-window) (- n (window-width)) t))

; http://ergoemacs.org/emacs/emacs_dired_tips.html
(define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)

(define-key dired-mode-map (kbd "^") (lambda () (interactive) (find-alternate-file "..")))

;; OSx ls does not support --dired; see ‘dired-use-ls-dired’ for more details.
;; https://stackoverflow.com/questions/25125200/emacs-error-ls-does-not-support-dired
(when (string= system-type "darwin")
  (setq dired-use-ls-dired nil))

;; ANSI & xterm-256 color text property translator for Emacs
;; suppose to be better than native ansi-color
;; https://github.com/atomontage/xterm-color
(use-package xterm-color
  :ensure t)

;; looks nice on screenshot but does not work for me on win
;; https://seagle0128.github.io/doom-modeline/
;; (use-package doom-modeline
;;       :ensure t
;;       :hook (after-init . doom-modeline-mode))

(setq-default mode-line-format
              '("%e" ; print error message about full memory.
                mode-line-front-space
                ;; mode-line-mule-info
                ;; mode-line-client
                ;; mode-line-modified
                ;; mode-line-remote
                ;; mode-line-frame-identification
                mode-line-buffer-identification
                "   "
                mode-line-position
                ;; (vc-mode vc-mode)
                ;; "  "
                ;; mode-line-modes
                "   "
                ;; mode-line-misc-info
                ;; battery-mode-line-string
                mode-line-end-spaces))

;; https://github.com/sabof/stripe-buffer
;; More easily visualize tabular data.
;; (use-package stripe-buffer :ensure t)

;; https://github.com/alpaker/Fill-Column-Indicator
(use-package fill-column-indicator
  :ensure t
  :config
  (setq fci-rule-column 79))
