(use-package benchmark-init
  :ensure t
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; packages archives
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'package-archives '("melpa-stable" . 
				 "https://stable.melpa.org/packages/") t)
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
;;(setq-default left-margin-width 0) ; Define new widths.
;;(set-window-buffer nil (current-buffer)) ; Use them now.
(when (display-graphic-p)
  (set-frame-font "PragmataPro Liga 14" nil t))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; general manual customizations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(blink-cursor-mode 0)
(column-number-mode 1)
(when (display-graphic-p)
  (scroll-bar-mode 0))
(setq inhibit-startup-screen t)
(setq ring-bell-function 'ignore)
(windmove-default-keybindings) ;; Use shift+arrow keys to shift windows
(put 'dired-find-alternate-file 'disabled nil)

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

(defun kill-and-close-other-window ()
      "Kill the other buffer and delete the selected window."
      (interactive)
      (when (> (length (window-list)) 1)
	  (other-window 1)
	  (kill-this-buffer)
	  (delete-window)))

(global-set-key [C-M-S-up] 'scroll-other-window-up-line)
(global-set-key [C-M-S-down] 'scroll-other-window-down-line)
(global-set-key (kbd "C-M-z") 'scroll-other-window-up-page)
(global-set-key (kbd "C-x M-k") 'kill-and-close-other-window)

(defun other-frame-previous ()
      "Select the previous frame, essentially calling other-frame with argument of -1"
      (interactive)
      (other-frame -1))
(global-set-key (kbd "C-c <right>") 'other-frame)
(global-set-key (kbd "C-c <left>") 'other-frame-previous)

(global-set-key (kbd "C-/") 'undo-only)
;; (global-set-key (kbd "C-\\") 'kill-whole-line)
(global-set-key (kbd "C-x C-a") 'kill-whole-line)
(global-set-key (kbd "C-x t l") 'tab-list)

(defun extended-command-other-window ()
    "Call M-x in the other window."
    (interactive)
    (save-selected-window
        (other-window 1)
        (execute-extended-command nil)))

(global-set-key (kbd "C-x 4 M-x") 'extended-command-other-window)

(global-set-key (kbd "M-s <left>") 'windmove-swap-states-left)
(global-set-key (kbd "M-s <right>") 'windmove-swap-states-right)
(global-set-key (kbd "M-s <up>") 'windmove-swap-states-up)
(global-set-key (kbd "M-s <down>") 'windmove-swap-states-down)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Split window for REPL programming
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun split-window-repl-programming ()
  "Split window vertically for REPL programming in a new frame. 
   Left half gets an extra 8% width."
  (interactive)
  (let* ((half-width (/ (window-total-width) 2))
	 (new-width (+ half-width
		       (* half-width 0.08))))
    (print new-width)
    ;;(make-frame '((name . "REPL-Frame")))
    (split-window-horizontally (round new-width))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Help with preferring to split windows side by side
;;;; Consult buffers are more ueful that way
;;;; On iPad BlinkSh terminals, emacs prefers to split up/down
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fix annoying vertical window splitting.
;; https://lists.gnu.org/archive/html/help-gnu-emacs/2015-08/msg00339.html
(with-eval-after-load "window"
  (defcustom split-window-below nil
    "If non-nil, vertical splits produce new windows below."
    :group 'windows
    :type 'boolean)

  (defcustom split-window-right nil
    "If non-nil, horizontal splits produce new windows to the right."
    :group 'windows
    :type 'boolean)

  (fmakunbound #'split-window-sensibly)

  (defun split-window-sensibly
      (&optional window)
    (setq window (or window (selected-window)))
    (or (and (window-splittable-p window t)
             ;; Split window horizontally.
             (split-window window nil (if split-window-right 'left  'right)))
        (and (window-splittable-p window)
             ;; Split window vertically.
             (split-window window nil (if split-window-below 'above 'below)))
        (and (eq window (frame-root-window (window-frame window)))
             (not (window-minibuffer-p window))
             ;; If WINDOW is the only window on its frame and is not the
             ;; minibuffer window, try to split it horizontally disregarding the
             ;; value of `split-width-threshold'.
             (let ((split-width-threshold 0))
               (when (window-splittable-p window t)
                 (split-window window nil (if split-window-right
                                              'left
                                            'right))))))))

(setq-default split-height-threshold  80
              split-width-threshold   160) ; the reasonable limit for horizontal splits



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LISP Executables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq inferior-lisp-program "ccl")
(setq scheme-program-name   "mit-scheme")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Use Packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package ace-window
  :ensure t
  :bind ("M-o" . ace-window)
  :custom (aw-dispatch-always t))

(use-package avy
  :ensure t
  :bind ("M-j" . avy-goto-char-timer)
  :custom (avy-timeout-seconds 1.0))

(use-package clojure-mode
  :ensure t
  :defer t
  :hook (clojure-mode . (lambda () (eldoc-mode -1))))

(use-package company
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
         ("M-g f" . consult-recent-file)           
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
   consult-theme :preview-key '(:debounce 0.3 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;;:preview-key "M-."
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
  :after consult)

;; (use-package eglot-java
;;   :ensure t
;;   :defer t
;;   :bind (("C-c l n" . eglot-java-file-new)
;; 	 ("C-c l x" . eglot-java-run-main)
;; 	 ("C-c l t" . eglot-java-run-test)
;; 	 ("C-c l N" . eglot-java-project-new)
;; 	 ("C-c l T" . eglot-java-project-build-task)
;; 	 ("C-c l R" . eglot-java-project-build-refresh))
;;   :hook (java-mode java-ts-mode))

(use-package embark
  :ensure t
  :bind  
  (("C-z" . embark-act)         ;; pick some comfortable binding
   ("C-c ," . embark-dwim)        ;; good alternative: M-.
   ("C-c b" . embark-bindings)) ;; alternative for `describe-bindings'
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  ;; Show the Embark target at point via Eldoc.  You may adjust the Eldoc
  ;; strategy, if you want to see the documentation from multiple providers.
  ;;(add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)
  :custom
  ;;(embark-quit-after-action '((kill-buffer . t) (t . nil)))
  (embark-quit-after-action nil)
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

;; (use-package envrc
;;   :hook (after-init . envrc-global-mode))

(use-package inf-clojure
     :ensure t
     :defer t
     :hook (inf-clojure-mode . (lambda () (eldoc-mode -1))))

(use-package kaolin-themes
  :ensure t
  :defer t
  ;;:config (load-theme 'kaolin-valley-dark t)
  )

(use-package lambda-themes
  :load-path "~/code/emacs/lambda-themes-mod"
  ;; :defer t
  :custom
  (lambda-themes-set-italic-comments nil)
  (lambda-themes-set-italic-keywords nil)
  (lambda-themes-set-variable-pitch nil) 
  :config
  ;; load preferred theme
  ;; (load-theme 'lambda-light)
  )

(use-package magit
  :ensure t
  :defer t)

(use-package marginalia
  :ensure t
  ;; Either bind `marginalia-cycle' globally or only in the minibuffer
  :bind ( ;;("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :init (marginalia-mode))

(use-package modus-themes
  :ensure t
  :defer nil
  :config
  (load-theme 'modus-operandi-tritanopia))

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
		   ;;basic-remote ; `tramp' hostname completion with `vertico'
		   partial-completion))))
  (orderless-component-separator 'orderless-escapable-split-on-space)
  (orderless-matching-styles
   '(orderless-prefixes     
     orderless-literal
     orderless-regexp
     orderless-initialism
     ;; orderless-flex
     ;; orderless-strict-leading-initialism
     ;; orderless-strict-initialism
     ;; orderless-strict-full-initialism
     ;; orderless-without-literal        ; Recommended for dispatches instead
     )))

(use-package paredit
  :ensure t
  :bind (:map paredit-mode-map
	 ("RET" . nil)
         ("C-j" . paredit-newline)
	 ("C-w" . paredit-kill-region)
	 ("C-\\" . backward-kill-sexp) 
	 ("M-s" . nil)
	 ("M-S" . nil)
	 ("C-<left>" . nil)
	 ("C-<right>" . nil)
	 ("C-M-y" . paredit-copy-as-kill)
	 ("{" . paredit-open-curly)
	 ("M-]" . paredit-forward-slurp-sexp)
	 ("M-r" . move-to-window-line-top-bottom)
	 ("M-s a" . paredit-raise-sexp)
	 ("M-s s" . paredit-splice-sexp)
	 ("M-s S" . paredit-split-sexp))
  :config (electric-indent-mode 0)
  :hook (clojure-mode scheme-mode inferior-scheme-mode 
		      emacs-lisp-mode ielm-mode sly-mrepl-mode
		      inf-clojure-mode inferior-lisp-mode lisp-mode))


(use-package recentf
  :ensure nil
  :bind 
  ("C-x C-r" . 'recentf-open-files)
  :custom 
  (recentf-max-menu-items 10)
  (recentf-max-saved-items 25)
  :config 
  (recentf-mode))

(use-package sly
  :ensure t
  :pin melpa 
  :init
  (setq sly-net-coding-system            'utf-8-unix
        sly-lisp-implementations         '((ccl ("~/bin/ccl"))
                                           (sbcl  ("/usr/local/bin/sbcl"))))
  :bind (:map sly-mode-map
	      ("C-c p" . sly-eval-print-last-expression)))

(use-package savehist 
;; Persist history over Emacs restarts. Vertico sorts by history position.  
  :config 
  (add-to-list 'savehist-additional-variables 'vertico-repeat-history)
  (savehist-mode))

(use-package vertico
  :ensure t
  :init
  (vertico-mode)
  ;; Different scroll margin
  ;; (setq vertico-scroll-margin 0)
  ;; Show more candidates
  (setq vertico-count 13)
  ;; Grow and shrink the Vertico minibuffer
  ;; (setq vertico-resize t)
  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  ;; (setq vertico-cycle t)
  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  (setq read-extended-command-predicate
        #'command-completion-default-include-p)
  :bind (:map vertico-map
	      ("C-M-n" .  #'vertico-next-group)
	      ("C-M-p" .  #'vertico-previous-group)))

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

(use-package vertico-multiform
  :after vertico
  :ensure nil
  :custom
  (vertico-buffer-display-action '(display-buffer-reuse-window))  
  (vertico-multiform-commands  '((consult-imenu buffer indexed)
				 (consult-line buffer indexed)
				 (consult-buffer buffer indexed)
				 (consult-buffer-other-window buffer indexed)
				 (consult-buffer-other-frame buffer indexed)
				 (t indexed)))
  (vertico-multiform-categories '((consult-grep buffer)
				  (buffer buffer indexed)
				  (command indexed)))
  :config
  (vertico-multiform-mode))

(use-package vertico-quick
  :after vertico
  :ensure nil
  :bind
  (:map vertico-map
	("C-q" . vertico-quick-insert)
	("M-q" . vertico-quick-exit)))

(use-package vertico-repeat
  :after vertico
  :ensure nil
  :bind ("C-c r" . vertico-repeat)
  :hook (minibuffer-setup . vertico-repeat-save))

(use-package vscode-dark-plus-theme
  :defer t
  :ensure t)

