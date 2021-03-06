;; These customizations make it easier for you to navigate files,
;; switch buffers, and choose options from the minibuffer.
;;
(global-set-key (kbd "C-x O") (lambda ()
                                (interactive)
                                (other-window -1)))
;; ;; Turn on recent file mode so that you can more easily switch to
;; ;; recently edited files when you first start emacs
;; (setq recentf-save-file (concat user-emacs-directory ".recentf"))
;; (require 'recentf)
;; (recentf-mode 1)
;; (setq recentf-max-menu-items 40)
;;
;;
;; ido-mode allows you to more easily navigate choices. For example,
;; when you want to switch buffers, ido presents you with a list
;; of buffers in the the mini-buffer. As you start to type a buffer's
;; name, ido will narrow down the list of buffers to match the text
;; you've typed in
;; http://www.emacswiki.org/emacs/InteractivelyDoThings
;; (ido-mode t)
;;
;; This allows partial matches, e.g. "tl" will match "Tyrion Lannister"
;; (setq ido-enable-flex-matching t)
;;
;; Turn this behavior off because it's annoying
;; (setq ido-use-filename-at-point nil)

;; Don't try to match file across all "work" directories; only match files
;; in the current directory displayed in the minibuffer
;; (setq ido-auto-merge-work-directories-length -1)
;;
;; ;; Includes buffer names of recently open files, even if they're not
;; ;; open now
;; (setq ido-use-virtual-buffers t)
;;
;; ;; This enables ido in all contexts where it could be useful, not just
;; ;; for selecting buffer and file names
;; (ido-ubiquitous-mode 1)
;;
;; ;; Shows a list of buffers
;; (global-set-key (kbd "C-x C-b") 'ibuffer)

(use-package flx
  :ensure t)

;; (use-package flx-ido
;;   :ensure t)

;; (use-package ido-vertical-mode
;;   :ensure t
;;   :config
;;   (ido-vertical-mode 1)
;;   (setq ido-vertical-define-keys 'C-n-and-C-p-only))

;; (use-package ido-completing-read+
;;   :ensure t
;;   :config
;;   (ido-mode t)
;;   (ido-ubiquitous-mode 1)
;;   ;; (setq ido-auto-merge-work-directories-length -1)
;;   )

;; (icomplete-mode t)
(fido-mode t)
(setq icomplete-separator "\n - ")
;; > Emacs 27.1
(setq completion-styles '(initials partial-completion flex))
(setq completion-cycle-threshold 10)
(setq read-buffer-completion-ignore-case t)

;; (define-key icomplete-minibuffer-map (kbd "C-n") 'icomplete-forward-completions)
;; (define-key icomplete-minibuffer-map (kbd "C-p") 'icomplete-backward-completions)
(define-key icomplete-fido-mode-map (kbd "C-n") 'icomplete-forward-completions)
(define-key icomplete-fido-mode-map (kbd "C-p") 'icomplete-backward-completions)

;; Enhances M-x to allow easier execution of commands. Provides
;; a filterable list of possible commands in the minibuffer
;; http://www.emacswiki.org/emacs/Smex
;; (use-package smex
;;   :ensure t
;;   :bind (("M-x" . smex))
;;   :config
;;   (setq smex-save-file (concat user-emacs-directory ".smex-items"))
;;   (smex-initialize))

;; projectile everywhere!
(use-package projectile
  :ensure t
  :bind (
         ("C-c p" . projectile-command-map)
         )
  :config
  (projectile-global-mode)
  (setq projectile-enable-caching t)
  )

(use-package ag
  :ensure t)

(defun transpose-windows ()
  "Transpose two windows.  If more or less than two windows are visible, error."
  (interactive)
  (unless (= 2 (count-windows))
    (error "There are not 2 windows."))
  (let* ((windows (window-list))
         (w1 (car windows))
         (w2 (nth 1 windows))
         (w1b (window-buffer w1))
         (w2b (window-buffer w2)))
    (set-window-buffer w1 w2b)
    (set-window-buffer w2 w1b)))

(put 'scroll-left 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
