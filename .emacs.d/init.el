(use-package benchmark-init
  :ensure t
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; packages archives
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(setq package-archive-priorities '(("melpa-stable"  . 30)
                                   ("gnu"    . 20)
				   ("nongnu" . 10)
                                   ("melpa"  . 0)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; custom file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; appearance customizations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(menu-bar-mode -1)
(setq-default left-margin-width 1) ; Define new widths.
(set-window-buffer nil (current-buffer)) ; Use them now.
(set-frame-font "PragmataPro Mono Liga 14" nil t)
;; (set-face-foreground 'font-lock-comment-face "#6A9955")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; general manual customizations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq inhibit-startup-screen t)
(setq ring-bell-function 'ignore)

(defun eshell/clear ()
  "Clear the eshell buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (eshell-send-input)))

;;(add-hook 'emacs-startup-hook 'eshell)
;; (add-hook 'emacs-startup-hook
;;           (lambda ()
;;             (cd "~/")
;;             (eshell)))

(setq desktop-path '("~/.emacs.d/"))
;;(desktop-save-mode 1)
(setq bookmark-save-flag 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Key customizations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "C-z") 'undo-only)
(global-set-key (kbd "C-S-z") 'undo-redo)

(defun scroll-other-window-up-line ()
  "Scroll the other window one line up."
  (interactive)
  (scroll-other-window -1))

(defun scroll-other-window-down-line ()
  "Scroll the other window one line down."
  (interactive)
  (scroll-other-window 1))

(defun scroll-other-window-up-page ()
  "Scroll the other window one page down."
  (interactive)
  (scroll-other-window -20))

(defun other-window-kill-buffer ()
  "Kill the buffer in the other window"
  (interactive)
  ;; Window selection is used because point goes to a different window
  ;; if more than 2 windows are present
  (let ((win-curr (selected-window))
        (win-other (next-window)))
    (select-window win-other)
    (kill-this-buffer)
    (select-window win-curr)))

(global-set-key [C-M-S-up] 'scroll-other-window-up-line)
(global-set-key [C-M-S-down] 'scroll-other-window-down-line)
(global-set-key (kbd "C-M-z") 'scroll-other-window-up-page)
(global-set-key (kbd "C-x M-k") 'other-window-kill-buffer)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LISP Executables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq inferior-lisp-program "ccl64")
(setq scheme-program-name   "/usr/local/bin/mit-scheme")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Use Packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package ace-window
  :ensure t
  :bind ("M-o" . ace-window)
  :custom (aw-dispatch-always t))

(use-package avy
  :ensure t
  :defer t)

(use-package clojure-mode
  :ensure t
  :defer t)

;; Example configuration for Consult
(use-package consult
  :ensure t

  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-recent-file)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi) 
         ;; M-s bindings in `search-map'
         ("M-s f" . consult-find)
         ("M-s D" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)
         ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
  ;;;; 1. project.el (the default)
  ;; (setq consult-project-function #'consult--default-project--function)
  ;;;; 2. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
  ;;;; 3. locate-dominating-file
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
  ;;;; 4. projectile.el (projectile-project-root)
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-function (lambda (_) (projectile-project-root)))
  ;;;; 5. No project support
  ;; (setq consult-project-function nil)

  ;; Use `consult-completion-in-region' if Vertico is enabled.
  ;; Otherwise use the default `completion--in-region' function.
  (setq completion-in-region-function
	(lambda (&rest args)
          (apply (if vertico-mode
                     #'consult-completion-in-region
                   #'completion--in-region)
		 args)))
)

(use-package consult-dir
  :ensure t
  :bind (("C-x C-d" . consult-dir)
         :map vertico-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file)))

(use-package consult-eglot
  :ensure t
  :defer t)

(use-package consult-project-extra
  :ensure t
  :defer t)

(use-package embark
  :ensure t
  :bind  
  (("C-c ." . embark-act)         ;; pick some comfortable binding
   ("C-c ," . embark-dwim)        ;; good alternative: M-.
   ("C-c b" . embark-bindings)) ;; alternative for `describe-bindings'
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  ;; Show the Embark target at point via Eldoc.  You may adjust the Eldoc
  ;; strategy, if you want to see the documentation from multiple providers.
  ;;(add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t ; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package inf-clojure
     :ensure t
     :defer t
     :hook (inf-clojure-mode . (lambda () (eldoc-mode -1))))

(use-package kaolin-themes
  :ensure t
  :defer t
  ;;:config (load-theme 'kaolin-valley-dark t)
  )

(use-package lambda-line
  ;;:disabled nil
  :load-path "~/code/lambda-line"
  :custom
  ;;(lambda-line-icon-time nil) ;; requires ClockFace font (see below)
  ;;(lambda-line-clockface-update-fontset "ClockFaceRect") ;; set clock icon
  (lambda-line-position 'top) ;; Set position of status-line 
  (lambda-line-abbrev t) ;; abbreviate major modes
  (lambda-line-hspace "")  ;; add some cushion
  (lambda-line-prefix t) ;; use a prefix symbol
  (lambda-line-prefix-padding nil) ;; no extra space for prefix 
  (lambda-line-status-invert nil)  ;; no invert colors
  (lambda-line-tty-ro-symbol  " RO") ;; symbols
  (lambda-line-tty-mod-symbol " MOD⬤") 
  (lambda-line-tty-rw-symbol  " RW") 
  (lambda-line-space-top +.50)  ;; padding on top and bottom of line
  (lambda-line-space-bottom -.50)
  (lambda-line-symbol-position 0.1) ;; adjust the vertical placement of symbol
  :config
  ;; activate lambda-line 
  (lambda-line-mode) 
  ;; set divider line in footer
  (when (eq lambda-line-position 'top)
    (setq-default mode-line-format (list "%_"))
    (setq mode-line-format (list "%_"))))

(use-package lambda-themes
  :load-path "~/code/lambda-themes"
  :custom
  (lambda-themes-set-italic-comments nil)
  (lambda-themes-set-italic-keywords nil)
  (lambda-themes-set-variable-pitch nil) 
  :config
  ;; load preferred theme 
  (load-theme 'lambda-light))

(use-package magit
  :ensure t
  :defer t)

(use-package marginalia
  ;; Either bind `marginalia-cycle' globally or only in the minibuffer
  :bind ( ;;("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :init (marginalia-mode))

(use-package multiple-cursors
  :ensure t
  :defer t)

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)    ; I want to be in control!
  (completion-category-overrides
   '((file (styles orderless
		   ;;basic-remote ; For `tramp' hostname completion with `vertico'
		   partial-completion))))
  (orderless-component-separator 'orderless-escapable-split-on-space)
  (orderless-matching-styles
   '(orderless-literal
     orderless-prefixes
     orderless-initialism
     orderless-regexp
     ;; orderless-flex
     ;; orderless-strict-leading-initialism
     ;; orderless-strict-initialism
     ;; orderless-strict-full-initialism
     ;; orderless-without-literal          ; Recommended for dispatches instead
     )))

(use-package paredit
  :ensure t
  :bind (:map paredit-mode-map
	 ("RET" . nil)
         ("C-j" . paredit-newline)
	 ("M-s" . nil)
	 ("M-S" . nil)
	 ("M-s s" . paredit-splice-sexp)
	 ("M-s S" . paredit-split-sexp)
	 )
  :config (electric-indent-mode 0)
  :hook (clojure-mode emacs-lisp-mode ielm-mode 
		       inf-clojure-mode inferior-lisp-mode lisp-mode))

(use-package rainbow-mode
  :ensure t)

(use-package recentf
  :ensure nil
  :bind 
  ("C-x C-r" . 'recentf-open-files)
  :custom 
  (recentf-max-menu-items 10)
  (recentf-max-saved-items 25)
  :config 
  (recentf-mode))

(use-package savehist 
;; Persist history over Emacs restarts. Vertico sorts by history position.
  :init (savehist-mode))

(use-package vertico
  :init
  (vertico-mode)
  ;; Different scroll margin
  ;; (setq vertico-scroll-margin 0)
  ;; Show more candidates
  ;;(setq vertico-count 20)
  ;; Grow and shrink the Vertico minibuffer
  ;; (setq vertico-resize t)
  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  ;; (setq vertico-cycle t)
  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  (setq read-extended-command-predicate
        #'command-completion-default-include-p)
  )

(use-package vertico-directory
  :after vertico
  :ensure nil
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package vscode-dark-plus-theme
  :defer t
  :ensure t)


  

