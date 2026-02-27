;;; init.el -*- lexical-binding: t; -*-
;; (setq debug-on-error t)
(setq gc-cons-threshold 300000000)
(setq gcmh-idle-delay 'auto  ; default is 15s
      gcmh-auto-idle-delay-factor 10
      gcmh-high-cons-threshold (* 64 1024 1024))  ; 64mb
;; (setq debug-on-error t)
(setq read-process-output-max (* 3(* 1024 1024))) ;; 1mb
(set-default-coding-systems 'utf-8)
(defvar elpaca-installer-version 0.11)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (let ((load-source-file-function nil)) (load "./elpaca-autoloads"))))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))
(elpaca-no-symlink-mode)
;; Install a package via the elpaca macro
;; See the "recipes" section of the manual for more details.

;; (elpaca example-package)

;; Install use-package support
(elpaca elpaca-use-package
  ;; Enable use-package :ensure support for Elpaca.
  (elpaca-use-package-mode))

;;When installing a package used in the init file itself,
;;e.g. a package which adds a use-package key word,
;;use the :wait recipe keyword to block until that package is installed/configured.
;;For example:
;;(use-package general :ensure (:wait t) :demand t)

;; Expands to: (elpaca evil (use-package evil :demand t))
(defconst my-auto-save-folder
  (expand-file-name "auto-saves/" user-emacs-directory))
(unless (file-exists-p my-auto-save-folder)
  (make-directory my-auto-save-folder t))
(setq auto-save-file-name-transforms
      `((".*" ,my-auto-save-folder t)))

(defconst IS-LINUX   (eq system-type 'gnu/linux))
(defconst IS-WINDOWS (memq system-type '(cygwin windows-nt ms-dos)))


(use-package emacs
  :ensure nil
  :custom
  (inhibit-splash-screen t)
  (pixel-scroll-mode t)
  (display-time-load-average nil)
  (idle-update-delay 0.01)
  (treesit-font-lock-level 4)
  (grep-command "grep --color=auto -nHr --null -e ")
  (visible-bell t)
  (create-lockfiles nil)
  (use-short-answers t)
  (ring-bell-function nil)
  (make-backup-files nil)
  (backup-by-copying nil)
  (auto-save-default nil)
  (inhibit-startup-message t)
  (inhibit-compacting-font-caches t)
  (delete-old-versions t)
  (kept-new-versions 6)
  (kept-old-versions 2)
  (version-control t)
  (scroll-margin 8)
  (mode-line-end-spaces nil)
  (blink-paren-function nil)
  (blink-matching-paren nil)
  (set-language-environment "UTF-8")
  (backup-by-copying t)    ; Don't delink hardlinks
  (version-control t)      ; Use version numbers on backups
  (delete-old-versions t)  ; Automatically delete excess backups
  (kept-new-versions 20)   ; how many of the newest versions to keep
  (kept-old-versions 5)    ; and how many of the old
  (backup-directory-alist '(("." . (concat user-emacs-directory "backup"))))
  (history-length 25)
  (use-dialog-box nil)
  (custom-theme-directory (concat user-emacs-directory "themes/"))
  :config
  (setq display-buffer-alist
        '(("\\*.*\\*" ; Match buffers like *Help*, *Compile*, etc.
           (display-buffer-reuse-window display-buffer-same-window))))

  (when IS-WINDOWS
    (setq find-program "c:/msys64/usr/bin/find.exe")
    )
  (setq custom-file (locate-user-emacs-file "custom-vars.el"))
  (load custom-file 'noerror 'nomessage)
  (global-auto-revert-mode 1)
  (setq-default display-line-numbers-type 'relative)
  (add-hook 'prog-mode-hook 'display-line-numbers-mode)
  (setq-default indent-tabs-mode nil
                tab-width 4
                fill-column 80)
  (global-display-fill-column-indicator-mode +1)

  (global-set-key [remap lookup-definition] #'xref-find-definitions)
  (global-set-key [remap sp/format-buffer] #'format-all-buffer)
  :init
  ;; Basic UI setup
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (savehist-mode +1)
  (save-place-mode 1)
  (set-fringe-mode 8)
  (display-battery-mode 1)
  )

(defun my/lookup-references ()
  "Use LSP references when available, otherwise fall back to xref."
  (interactive)
  (cond
   ;; Prefer lsp-find-references when lsp-mode is enabled or there are buffer workspaces
   ((and (fboundp #'lsp-find-references)
         (or (bound-and-true-p lsp-mode)
             ;; check internal workspace list as a fallback indicator
             (and (boundp 'lsp--buffer-workspaces)
                  lsp--buffer-workspaces)))
    (call-interactively #'lsp-find-references))

   ;; Otherwise use xref
   ((fboundp #'xref-find-references)
    (call-interactively #'xref-find-references))

   (t (user-error "No references command available"))))

(defun my/lookup-definitions ()
  "Use LSP references when available, otherwise fall back to xref."
  (interactive)
  (cond
   ;; Prefer lsp-find-references when lsp-mode is enabled or there are buffer workspaces
   ((and (fboundp #'lsp-)
         (or (bound-and-true-p lsp-mode)
             ;; check internal workspace list as a fallback indicator
             (and (boundp 'lsp--buffer-workspaces)
                  lsp--buffer-workspaces)))
    (call-interactively #'lsp-find-definition))

   ;; Otherwise use xref
   ((fboundp #'xref-find-references)
    (call-interactively #'xref-find-definitions))

   (t (user-error "No references command available"))))

(defun sp/new-frame ()
  (set-face-attribute 'default nil :font (font-spec :family "BerkeleyMonoVariable Nerd Font") :height 140 :weight 'medium)
  (set-face-attribute 'fixed-pitch nil :font (font-spec :family "BerkeleyMonoVariable Nerd Font") :height 140 :weight 'medium)
  (when IS-WINDOWS
    (set-frame-parameter (selected-frame) 'alpha '(98 . 98))
    ;; (set-frame-parameter (selected-frame) 'alpha-background 0.9 )
    ;; (add-to-list 'default-frame-alist '(alpha . (98 . 98)))
    )
  (when IS-LINUX
    (set-frame-parameter (selected-frame) 'alpha-background 0.9 ))
  )

(defun unicode-fonts-setup-h (frame)
  "Run unicode-fonts-setup, then remove the hook."
  (when (and frame (display-graphic-p frame))
    (with-selected-frame frame
      (require 'unicode-fonts)
      (unicode-fonts-setup)
      (sp/new-frame)
      )))

(use-package dashboard
  :ensure t
  :init
  (setq dashboard-display-icons-p t)
  (setq initial-buffer-choice 'dashboard-open
        dashboard-center-content nil
        dashboard-vertically-center-content t)
  (setq dashboard-icon-type 'nerd-icons)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)

  (setq dashboard-items '((projects . 5)
                          (recents . 5 )
                          (bookmarks . 3)
                          (agenda . 3)))
  (setq dashboard-startup-banner (concat user-emacs-directory "images/emacs-dash.png"))  ;; use custom image as banner
  :config
  (add-hook 'elpaca-after-init-hook #'dashboard-insert-startupify-lists)
  (add-hook 'elpaca-after-init-hook #'dashboard-initialize)
  (add-hook 'server-after-make-frame-hook 'dashboard-open)
  (dashboard-setup-startup-hook))

(use-package pulsar :ensure t :demand t
  :init (pulsar-global-mode))

(defvar initial-compilation t
  "Non-nil if this is the initial compilation.")
(setq initial-compilation t)

(defun compile-or-recompile ()
  "Compile or recompile based on the value of `initial-compilation'.
If `initial-compilation' is non-nil, call `project-compile' and set it to nil.
If nil, call `project-recompile'."
  (interactive)
  (if initial-compilation
      (progn
        (project-compile)
        (setq initial-compilation nil))
    (project-recompile)))

(defun reset-initial-compilation (&rest _)
  "Reset `initial-compilation' when switching projects."
  (setq initial-compilation t))

(advice-add 'project-switch-project :before #'reset-initial-compilation)
(use-package evil :ensure t :demand t
  :config
  (defun sp/evil-yank-advice (orig-fn beg end &rest args)
    (require 'pulsar)
    (pulsar-highlight-pulse (cons beg end))
    (apply orig-fn beg end args))

  (advice-add 'evil-yank :around 'sp/evil-yank-advice)
  (define-key evil-normal-state-map (kbd "g r") nil)
  (evil-global-set-key 'normal (kbd "C-M-u") 'universal-argument)
  (evil-global-set-key 'normal (kbd "g d") 'my/lookup-definitions)
  (evil-global-set-key 'normal (kbd "g i") 'lookup-implementation)
  (evil-global-set-key 'normal (kbd "g r") nil)
  (evil-global-set-key 'normal (kbd "g r r") 'my/lookup-references)
  (evil-global-set-key 'normal (kbd "g t") 'lookup-type-definition)
  (evil-global-set-key 'normal (kbd "g c c") 'comment-line)
  (evil-global-set-key 'visual (kbd "g c") 'comment-or-uncomment-region)
  (evil-global-set-key 'insert (kbd "C-p") nil)
  (evil-global-set-key 'insert (kbd "C-j") nil)
  (evil-global-set-key 'insert (kbd "C-k") nil)
  (evil-global-set-key 'insert (kbd "C-h") nil)
  (evil-global-set-key 'insert (kbd "C-l") nil)
  (evil-global-set-key 'normal (kbd "C-p") nil)
  (evil-global-set-key 'normal (kbd "C-u") 'evil-scroll-up)
  (evil-global-set-key 'normal (kbd "K") nil)
  (evil-global-set-key 'normal (kbd "J") nil)
  (evil-global-set-key 'normal (kbd "C-j") 'windmove-down)
  (evil-global-set-key 'normal (kbd "C-k") 'windmove-up)
  (evil-global-set-key 'normal (kbd "C-h") 'windmove-left)
  (evil-global-set-key 'normal (kbd "C-l") 'windmove-right)
  (evil-global-set-key 'normal "-" 'dired-jump)
  (evil-global-set-key 'normal (kbd "M-.") 'consult-project-extra-find)
  (evil-global-set-key 'normal (kbd "\\") 'evil-window-vsplit)
  (evil-global-set-key 'normal (kbd "C-+") 'text-scale-increase)
  (evil-global-set-key 'normal (kbd "C--") 'text-scale-decrease)
  (evil-global-set-key 'normal (kbd "C-f") 'project-switch-project)
  ;; (evil-global-set-key 'normal (kbd "<f5>") 'compile-or-recompile)
  (evil-global-set-key 'normal (kbd "<f5>") #'compile-or-recompile)
  (evil-define-key 'normal eshell-mode-map (kbd "C-j") 'windmove-down)
  (evil-define-key 'normal eshell-prompt-mode-map (kbd "C-j") 'windmove-down)
  (evil-define-key 'normal eshell-mode-map (kbd "C-k") 'windmove-up)
  (evil-define-key 'normal eshell-prompt-mode-map (kbd "C-k") 'windmove-up)
  (evil-define-key 'normal eshell-mode-map (kbd "C-h") 'windmove-left)
  (evil-define-key 'normal eshell-prompt-mode-map (kbd "C-h") 'windmove-left)
  (evil-define-key 'normal eshell-mode-map (kbd "C-l") 'windmove-right)
  (evil-define-key 'normal eshell-prompt-mode-map (kbd "C-l") 'windmove-right)
  (evil-define-key 'normal shell-mode-map (kbd "C-j") 'windmove-down)
  (evil-define-key 'normal shell-mode-map (kbd "C-k") 'windmove-up)
  (evil-define-key 'normal shell-mode-map (kbd "C-h") 'windmove-left)
  (evil-define-key 'normal shell-mode-map (kbd "C-l") 'windmove-right)
  :init      ;; tweak evil's configuration before loading it
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil)
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below t)
  (evil-mode))

(use-package evil-collection
  :ensure t :demand t
  :after evil
  :config
  (setq evil-collection-key-blacklist '("C-h" "C-j" "C-k" "C-l" ))
  (evil-collection-init))

(use-package evil-multiedit
  :ensure t :demand t
  :commands (evil-mc-make-cursor-here
             evil-mc-make-all-cursors
             evil-mc-undo-all-cursors
             evil-mc-pause-cursors
             evil-mc-resume-cursors
             evil-mc-make-and-goto-first-cursor
             evil-mc-make-and-goto-last-cursor
             evil-mc-make-cursor-in-visual-selection-beg
             evil-mc-make-cursor-in-visual-selection-end
             evil-mc-make-cursor-move-next-line
             evil-mc-make-cursor-move-prev-line
             evil-mc-make-cursor-at-pos
             evil-mc-has-cursors-p
             evil-mc-make-and-goto-next-cursor
             evil-mc-skip-and-goto-next-cursor
             evil-mc-make-and-goto-prev-cursor
             evil-mc-skip-and-goto-prev-cursor
             evil-mc-make-and-goto-next-match
             evil-mc-skip-and-goto-next-match
             evil-mc-skip-and-goto-next-match
             evil-mc-make-and-goto-prev-match
             evil-mc-skip-and-goto-prev-match)
  :config
  (add-hook 'evil-mc-before-cursors-created #'evil-mc-pause-incompatible-modes)
  (add-hook 'evil-mc-before-cursors-created #'evil-mc-initialize-active-state)
  (add-hook 'evil-mc-after-cursors-deleted  #'evil-mc-teardown-active-state)
  (add-hook 'evil-mc-after-cursors-deleted  #'evil-mc-resume-incompatible-modes)
  (advice-add #'evil-mc-initialize-hooks :override #'ignore)
  (advice-add #'evil-mc-teardown-hooks :override #'evil-mc-initialize-vars)
  (advice-add #'evil-mc-initialize-active-state :before #'turn-on-evil-mc-mode)
  (advice-add #'evil-mc-teardown-active-state :after #'turn-off-evil-mc-mode))

(use-package project :ensure t
  :custom
  (project-vc-extra-root-markers '(".jj"))
  :config
  (add-to-list 'project-vc-backend-markers-alist '(JJ . ".jj")))

(use-package evil-mc
  :ensure t :demand t
  :config
  ;; evil-multiedit
  (evil-define-key 'normal 'global
    (kbd "M-b")   #'evil-multiedit-match-symbol-and-next
    (kbd "M-B")   #'evil-multiedit-match-symbol-and-prev)
  (evil-define-key 'visual 'global
    "R"           #'evil-multiedit-match-all
    (kbd "M-b")   #'evil-multiedit-match-and-next
    (kbd "M-B")   #'evil-multiedit-match-and-prev)
  (evil-define-key '(visual normal) 'global
    (kbd "C-M-b") #'evil-multiedit-restore)

  (with-eval-after-load 'evil-mutliedit
    (evil-define-key 'multiedit 'global
      (kbd "M-b")   #'evil-multiedit-match-and-next
      (kbd "M-S-b") #'evil-multiedit-match-and-prev
      (kbd "RET")   #'evil-multiedit-toggle-or-restrict-region)
    (evil-define-key '(multiedit multiedit-insert) 'global
      (kbd "C-n")   #'evil-multiedit-next
      (kbd "C-p")   #'evil-multiedit-prev))

  ;; evil-mc
  (evil-define-key '(normal visual) 'global
    "gzm" #'evil-mc-make-all-cursors
    "gzu" #'evil-mc-undo-all-cursors
    "gzn" #'evil-mc-make-and-goto-next-cursor
    "gzp" #'evil-mc-make-and-goto-prev-cursor
    "gzN" #'evil-mc-make-and-goto-last-cursor
    "gzP" #'evil-mc-make-and-goto-first-cursor)
  (with-eval-after-load 'evil-mc
    (evil-define-key '(normal visual) evil-mc-key-map
      (kbd "C-n") #'evil-mc-make-and-goto-next-cursor
      (kbd "C-N") #'evil-mc-make-and-goto-last-cursor
      (kbd "C-p") #'evil-mc-make-and-goto-prev-cursor
      (kbd "C-P") #'evil-mc-make-and-goto-first-cursor)))

(use-package cape
  :ensure t :demand t
  ;; Bind dedicated completion commands
  ;; Alternative prefix keys: C-c p, M-p, M-+, ...
  :bind (("C-c p p" . completion-at-point) ;; capf
	     ("C-c p t" . complete-tag)        ;; etags
	     ("C-c p d" . cape-dabbrev)        ;; or dabbrev-completion
	     ("C-c p h" . cape-history)
	     ("C-c p f" . cape-file)
	     ("C-c p k" . cape-keyword)
	     ("C-c p s" . cape-elisp-symbol)
	     ("C-c p e" . cape-elisp-block)
	     ("C-c p a" . cape-abbrev)
	     ("C-c p l" . cape-line)
	     ("C-c p w" . cape-dict)
	     ("C-c p :" . cape-emoji)
	     ("C-c p \\" . cape-tex)
	     ("C-c p _" . cape-tex)
	     ("C-c p ^" . cape-tex)
	     ("C-c p &" . cape-sgml)
	     ("C-c p r" . cape-rfc1345))
  :init
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.  The order of the functions matters, the
  ;; first function returning a result wins.  Note that the list of buffer-local
  ;; completion functions takes precedence over the global list.
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  ;; (add-to-list 'completion-at-point-functions #'cape-history)
  ;; (add-to-list 'completion-at-point-functions #'cape-keyword)
  ;; (add-to-list 'completion-at-point-functions #'cape-tex)
  ;; (add-to-list 'completion-at-point-functions #'cape-sgml)
  ;; (add-to-list 'completion-at-point-functions #'cape-rfc1345)
  ;; (add-to-list 'completion-at-point-functions #'cape-abbrev)
  ;; (add-to-list 'completion-at-point-functions #'cape-dict)
  ;; (add-to-list 'completion-at-point-functions #'cape-elisp-symbol)
  ;; (add-to-list 'completion-at-point-functions #'cape-line)

  (setq-local completion-at-point-functions
		      (list (cape-capf-buster #'some-caching-capf)))
  )

(use-package vertico
  :ensure t :demand t
  :custom
  (vertico-scroll-margin 0) ;; Different scroll margin
  (vertico-count 20) ;; Show more candidates
  (vertico-resize t) ;; Grow and shrink the Vertico minibuffer
  (vertico-cycle t) ;; Enable cycling for `vertico-next/previous'
  :init
  (vertico-mode))

(with-eval-after-load 'vertico
  (define-key vertico-map (kbd "C-q") 'embark-export)
  (define-key vertico-map (kbd "C-a") 'embark-act)
  )

(use-package vertico-posframe
  :ensure t :demand t
  :after (vertico)
  :init
  (vertico-posframe-mode)
  :config
  (setq vertico-posframe-parameters
        '((left-fringe . 8)
          (top-fringe . 8)
          (bottom-fringe . 8)
          (right-fringe . 8))))

(use-package consult
  :ensure t :demand t
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)
  (advice-add #'register-preview :override #'consult-register-window)
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :config
  (setq consult-preview-key "M-,")
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   :preview-key '(:debounce 0.4 any))
  (setq consult-narrow-key "<"))

(use-package embark
  :ensure t :demand t
  :config
  (evil-define-key 'normal collect-mode-map (kbd "C-j") 'windmove-down)
  (evil-define-key 'normal collect-mode-map (kbd "C-k") 'windmove-up)
  (evil-define-key 'normal collect-mode-map (kbd "C-h") 'windmove-left)
  (evil-define-key 'normal collect-mode-map (kbd "C-l") 'windmove-right)
  (evil-define-key 'normal embark-collect-mode-map (kbd "C-j") 'windmove-down)
  (evil-define-key 'normal embark-collect-mode-map (kbd "C-k") 'windmove-up)
  (evil-define-key 'normal embark-collect-mode-map (kbd "C-h") 'windmove-left)
  (evil-define-key 'normal embark-collect-mode-map (kbd "C-l") 'windmove-right)
  )

(use-package embark-consult
  :ensure t ; only need to install it, embark loads it after consult if found
  :hook
  ((embark-collect-mode . consult-preview-at-point-mode)))

(use-package corfu
  :ensure t
  :custom
  (corfu-auto t)
  (corfu-cycle t)
  (corfu-preselect 'first)
  (corfu-auto-prefix 2)
  (corfu-popupinfo-delay '(0.1 . 0.2))
  (corfu-auto-delay 0)
  (corfu-quit-at-boundary 'separator)
  (corfu-preview-current 'insert)
  :init
  (global-corfu-mode 1)
  (corfu-history-mode 1)
  (corfu-popupinfo-mode 1))

(with-eval-after-load 'corfu
  (evil-define-key 'insert corfu-map (kbd "C-y") #'corfu-insert)
  (evil-define-key 'insert corfu-map (kbd "TAB") nil)
  (evil-define-key 'insert corfu-map (kbd "<tab>") nil)
  (evil-define-key 'insert corfu-map (kbd "RET") nil)
  (evil-define-key 'insert corfu-map (kbd "<return>") nil))

(use-package marginalia
  :ensure t :demand t
  :after vertico
  :init
  (marginalia-mode))

(use-package orderless
  :ensure t :demand t
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles basic partial-completion)))))

(use-package undo-tree
  :ensure t :demand t
  :after evil
  :custom (undo-tree-history-directory-alist `(("." . ,(concat user-emacs-directory "var/undo-tree-hist/"))))
  :config
  (evil-global-set-key 'normal (kbd "u") 'undo-tree-undo)
  (evil-global-set-key 'normal (kbd "C-r") 'undo-tree-redo)
  (setq undo-tree-visualizer-diff t
        undo-tree-auto-save-history t
        undo-tree-enable-undo-in-region t
        ;; Increase undo limits to avoid emacs prematurely truncating the undo
        ;; history and corrupting the tree. This is larger than the undo-fu
        ;; defaults because undo-tree trees consume exponentially more space,
        ;; and then some when `undo-tree-enable-undo-in-region' is involved. See
        ;; syl20bnr/spacemacs#12110
        undo-limit 800000           ; 800kb (default is 160kb)
        undo-strong-limit 12000000  ; 12mb  (default is 240kb)
        undo-outer-limit 128000000) ; 128mb (default is 24mb)
  :init (global-undo-tree-mode))

(use-package no-littering
  :ensure t :demand t
  )
(use-package doom-themes
  :ensure t :demand t
  :init
  (load-theme 'gruvbox-sp t))

(use-package doom-modeline
  :ensure t :demand t
  :init (doom-modeline-mode))

(use-package rainbow-delimiters
  :ensure (rainbow-delimiters :type git :host github :repo "samwdp/rainbow-delimiters")
  :demand t
  :config
  :hook ((emacs-lisp-mode . rainbow-delimiters-mode)
         (csharp-ts-mode . rainbow-delimiters-mode)
         (rust-ts-mode . rainbow-delimiters-mode)
         (odin-ts-mode . rainbow-delimiters-mode)
         (typescript-ts-mode . rainbow-delimiters-mode)
         (css-ts-mode . rainbow-delimiters-mode)
         (scss-ts-mode . rainbow-delimiters-mode)
         (html-ts-mode . rainbow-delimiters-mode)
         (json-ts-mode . rainbow-delimiters-mode)
         (yaml-ts-mode . rainbow-delimiters-mode)
         ))

(use-package rainbow-mode
  :ensure t :demand t
  :hook (prog-mode . rainbow-mode))

(use-package which-key
  :ensure t :demand t
  :init (which-key-mode))

(use-package windmove
  :ensure nil
  :config
  (setq windmove-wrap-around t))

(use-package general
  :ensure t :demand t
  :config
  (general-evil-setup)
  (general-create-definer sp/leader-keys
    :prefix "SPC"
    )
  (general-create-definer sp/leader-keys-local
    :prefix "SPC c"
    :wk "Local Leader"
    )
  (sp/leader-keys-local
    :states 'normal
    :keymaps 'html-ts-mode-map
    "n" '(sgml-skip-tag-forward :wk "Find Closing Tag")
    "p" '(sgml-skip-tag-backward :wk "Find Opening Tag")
    )
  (sp/leader-keys-local
    :states 'normal
    :keymaps 'csharp-ts-mode-map
    "s" '(sharper-main-transient :wk "[O]pen [S]harper")
    )
  (sp/leader-keys-local
    :states 'normal
    :keymaps 'emacs-lisp-mode-map
    "b" '(eval-buffer :wk "[E]val [B]uffer")
    "d" '(eval-defun :wk "[E]val [D]efun")
    "r" '(eval-region :wk "[E]val [R]egion")
    )
  (sp/leader-keys
    :keymaps 'visual
    "ar" '(eca-rewrite :wk "[A]i [R]ewrite")
    )
  (sp/leader-keys
    :keymaps 'normal
    ;; single use keymaps
    "." '(find-file :wk "find files")
    "SPC" '(consult-project-extra-find-other-window :wk "find files")
    "f" '(sp/format-buffer :wk "format buffer")
    "w" '(save-buffer :wk "save")
    ;; ai
    "a" '(:ignore t :wk "[A]I")
    "aa" '(eca :wk "[A]I [A]sk")
    "at" '(eca-transient-menu :wk "[A]i [T]ransient")
    ;; buffers
    "b" '(:ignore t :wk "buffer")
    "bb" '(consult-project-buffer :wk "Switch buffer")
    "bd" '(kill-this-buffer :wk "Switch buffer")
    "bB" '(consult-buffer :wk "all buffers")
    "bk" '(kill-this-buffer :wk "Kill this buffer")
    "bn" '(next-buffer :wk "Next buffer")
    "bp" '(previous-buffer :wk "Previous buffer")
    "br" '(revert-buffer :wk "Reload buffer")
    ;; delete
    "d" '(:ignore t :wk "[D]elete")
    "db" '(kill-current-buffer :wk "[D]elete [B]uffer")
    "dw" '(delete-window :wk "[D]elete [W]indow")
    "h" '(:ignore t :wk "[H]arpoon")
    "ha" '(harpoon-add-file :wk "[H]arpoon [A]dd")
    "he" '(harpoon-toggle-quick-menu :wk "[H]arpoon [E]dit")
    "hc" '(harpoon-clear :wk "[H]arpoon [C]lear")
    ;; git
    "g" '(:ignore t :wk "[G]it")
    "gs" '(magit-status :wk "[G]it [S]tatus")
    "gj" '(jj-log :wk "[J]ujitsu [L]og")
    ;; instert
    "i" '(:ignore t :wk "[I]nsert")
    "is" '(consult-yasnippet :wk "[I]nsert [S]nippet")
    ;; open things
    "o" '(:ignore t :wk "[O]pen")
    "ob" '(dashboard-open t :wk "[O]pen [D]ired")
    "od" '(dired t :wk "[O]pen [D]ired")
    "oe" '(project-eshell t :wk "[O]pen [E]shell")
    "ot" '(eat-project t :wk "[O]pen [T]erminal")
    "os" '(eat t :wk "[O]pen [T]erminal")
    ;; projects
    "p" '(:ignore t :wk "[P]erspective")
    "pd" '(project-discover-run :wk "[P]roject [D]iscover")
    "pk" '(persp-kill :wk "[P]erspective [K]ill")
    "ps" '(persp-switch :wk "[P]erspective [S]witch")
    "pp" '(popper-toggle :wk "[P]opup [T]oggle")
    "pn" '(popper-cycle :wk "[P]opup [N]ext")
    ;; search
    "s" '(:ignore t :wk "[S]earch")
    "sd" '(consult-lsp-diagnostics :wk "[S]earch [D]iagnostics")
    "sg" '(consult-ripgrep :wk "[S]earch [G]rep")
    "ss" '(consult-lsp-symbols :wk "[S]earch [G]rep")
    ;; toggles
    "t" '(:ignore t :wk "[T]oggle")
    "tb" '(toggle-big-font :wk "[T]oggle [B]ig Font Mode")
    )
  (general-define-key
   "C-f" '(project-switch-project :wk "switch project")
   "C-+" 'text-scale-increase
   (kbd "C--") 'text-scale-increase
   "C-M-n" 'harpoon-go-to-1
   "C-M-e" 'harpoon-go-to-2
   "C-M-o" 'harpoon-go-to-3
   "C-M-i" 'harpoon-go-to-4
   "C-M-=" 'harpoon-toggle-file
   "C-S-d" 'popper-raise-popup
   "C-h" 'windmove-left
   "C-l" 'windmove-right
   "C-k" 'windmove-up
   "C-j" 'windmove-down))

(use-package dtrt-indent
  :demand t :ensure t  )

(use-package posframe
  :demand t :ensure t  )
(use-package transient
  :demand t :ensure t  )

(use-package perspective
  :ensure t :demand t
  :hook (elpaca-after-init . persp-mode)
  :custom
  (persp-mode-prefix-key (kbd "C-c C-p"))
  :config
  (setq persp-modestring-dividers '(" "))
  (setq persp-nil-name "main"
        persp-modestring-short t
        persp-set-last-persp-for-new-frames t))

(use-package perspective-project-bridge
  :ensure t
  :hook
  (perspective-project-bridge-mode . (lambda ()
                                       (if perspective-project-bridge-mode
                                           (perspective-project-bridge-find-perspectives-for-all-buffers)
                                         (perspective-project-bridge-kill-perspectives))))
  (persp-mode . perspective-project-bridge-mode)
  :config
  (defvar perspective-project-bridge-separator "/")
  (defvar perspective-project-bridge-depth 3)
  (defun my-persp-project-name-from-path (project-root)
    "Generate a perspective name from PROJECT-ROOT path."
    (let* ((parts (split-string (directory-file-name project-root) "[/\\]" t))
           (n (length parts)))
      ;; Always include up to 3 last parts: project, feature, branch
      (mapconcat #'identity (last parts (min perspective-project-bridge-depth n)) perspective-project-bridge-separator)))

  (defun perspective-project-bridge-find-perspective-for-buffer (buffer)
    "Find a project-specific perspective for BUFFER.
If no such perspective exists, a new one is created and the buffer is added to it."
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when (and perspective-project-bridge-mode
                   (buffer-name buffer)
                   (project-current))
          (let* ((project-root (directory-file-name
                                (if (fboundp 'project-root)
                                    (project-root (project-current))
                                  (car (project-roots (project-current))))))
                 (name (my-persp-project-name-from-path project-root))
                 (persp (persp-new name)))
            (with-perspective (persp-name persp)
              (setq perspective-project-bridge-persp t)
              (persp-add-buffer buffer))
            persp)))))
  )

(use-package eat
  :ensure (eat :repo "https://codeberg.org/samwdp/emacs-eat" :branch "windows-hack")
  :config
  (evil-define-key 'visual eat-mode-map (kbd "y") #'evil-yank)
  (evil-define-key 'normal eat-mode-map (kbd "p") #'evil-yank))


(use-package hl-todo
  :ensure t :demand t
  :hook ((prog-mode . hl-todo-mode)
         (fundamental-mode . hl-todo-mode)
         (org-mode . hl-todo-mode)
         (git-commit-mode . hl-todo-mode))
  :config
  (setq hl-todo-highlight-punctuation ":"
        hl-todo--regexp "\\(\\<\\(HOTFIX\\|hotfix\\|FIX\\|fix\\|FEAT\\|feat\\|TODO\\|todo\\|FIXME\\|fixme\\|HACK\\|hack\\|REVIEW\\|review\\|NOTE\\|note\\|DEPRECATED\\|deprecated\\|BUG\\|bug\\|XXX\\)\\>[:]*\\)"
        hl-todo-keyword-faces
        `(;; For things that need to be done, just not today.
          ("feat" font-lock-function-call-face bold)
          ("FEAT" font-lock-function-call-face bold)
          ("TODO" warning bold)
          ("todo" warning bold)
          ;; For problems that will become bigger problems later if not
          ;; fixed ASAP.
          ("hotfix" error bold)
          ("HOTFIX" error bold)
          ("FIXME" error bold)
          ("fixme" error bold)
          ("FIX" error bold)
          ("fix" error bold)
          ;; For tidbits that are unconventional and not intended uses of the
          ;; constituent parts, and may break in a future update.
          ("HACK" font-lock-constant-face bold)
          ("hack" font-lock-constant-face bold)
          ;; For things that were done hastily and/or hasn't been thoroughly
          ;; tested. It may not even be necessary!
          ("REVIEW" font-lock-keyword-face bold)
          ("review" font-lock-keyword-face bold)
          ;; For especially important gotchas with a given implementation,
          ;; directed at another user other than the author.
          ("NOTE" success bold)
          ("note" success bold)
          ;; For things that just gotta go and will soon be gone.
          ("DEPRECATED" font-lock-doc-face bold)
          ("deprecated" font-lock-doc-face bold)
          ;; For a known bug that needs a workaround
          ("BUG" error bold)
          ("bug" error bold)
          ;; For warning about a problematic or misguiding code
          ("XXX" font-lock-constant-face bold))))

(use-package git-gutter
  :ensure t :demand t
  :hook (prog-mode . git-gutter-mode))

(use-package git-gutter-fringe
  :ensure t :demand t
  :config
  (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom))

(use-package golden-ratio
  :ensure t :demand t
  :init
  (golden-ratio-mode +1))

(use-package unicode-fonts
  :ensure t :demand t
  :init
  (if (display-graphic-p)
      (unicode-fonts-setup-h (selected-frame))
    (add-hook 'after-make-frame-functions 'unicode-fonts-setup-h)))

(use-package ligature
  :ensure t :demand t
  :config
  ;; Enable the "www" ligature in every possible major mode
  (ligature-set-ligatures 't '("www"))
  ;; Enable traditional ligature support in eww-mode, if the
  ;; `variable-pitch' face supports it
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  ;; Enable all Cascadia Code ligatures in programming modes
  (ligature-set-ligatures 'prog-mode '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
                                       ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
                                       "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
                                       "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
                                       "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
                                       "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
                                       "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
                                       "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
                                       ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
                                       "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
                                       "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
                                       "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
                                       "\\\\" "://"))
  ;; Enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
  (global-ligature-mode t))

(use-package nerd-icons
  :ensure t :demand t
  )

(use-package nerd-icons-completion
  :ensure t :demand t
  :after marginalia
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

(use-package kind-icon
  :ensure t
  :after corfu
  :custom
  (kind-icon-use-icons t)
  (kind-icon-default-face 'corfu-default) ; Have background color be the same as `corfu' face background
  (kind-icon-blend-background nil)  ; Use midpoint color between foreground and background colors ("blended")?
  (kind-icon-blend-frac 0.08)
  (svg-lib-icons-dir (no-littering-expand-var-file-name "svg-lib/cache/")) ; Change cache dir
  :config
  (setq kind-icon-default-style
        '(:padding 0 :stroke 0 :margin 0 :radius 0 :height 0.6 :scale 1.0 :background
                   nil)) ;; hack to fix overflowing icons on corfu

  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package treemacs-nerd-icons
  :ensure t :demand t
  :config
  (treemacs-load-theme "nerd-icons"))

(use-package smartparens :ensure t :demand t)

(use-package adaptive-wrap
  :ensure t)

(use-package adaptive-word-wrap-mode
  :ensure (adaptive-word-wrap-mode :type git :host github :repo "samwdp/adaptive-word-wrap-mode")
  :hook (elpaca-after-init . global-adaptive-word-wrap-mode))

(defvar sp/keys-keymap (make-keymap)
  "Keymap for my/keys-mode")

(define-minor-mode sp/keys-mode
  "Minor mode for my personal keybindings."
  :init-value t
  :global t
  :keymap sp/keys-keymap)

;; The keymaps in `emulation-mode-map-alists' take precedence over
;; `minor-mode-map-alist'
(add-to-list 'emulation-mode-map-alists
             `((sp/keys-mode . ,sp/keys-keymap)))

(define-key sp/keys-keymap (kbd "C-j") 'windmove-down)
(define-key sp/keys-keymap (kbd "C-h") 'windmove-left)
(define-key sp/keys-keymap (kbd "C-k") 'windmove-up)
(define-key sp/keys-keymap (kbd "C-l") 'windmove-right)

(use-package drag-stuff
  :ensure t
  :config
  (evil-global-set-key 'visual (kbd "J")
                       (lambda (arg) (interactive "p") (drag-stuff-down arg)
                         (format-all-region-or-buffer)))
  (evil-global-set-key 'visual (kbd "K")
                       (lambda (arg) (interactive "p") (drag-stuff-up arg)
                         (format-all-region-or-buffer)))
  :init
  (drag-stuff-global-mode +1))


(use-package consult-project-extra
  :ensure t :demand t
  :bind
  (("C-c p f" . consult-project-extra-find)
   ("C-c p o" . consult-project-extra-find-other-window)))



;;;###autoload
(defun lsp/switch-client (client)
  "Switch to another LSP server CLIENT for the current buffer."
  (interactive
   (progn
     (require 'lsp-mode)
     (list (completing-read
            "Select server: "
            (or (mapcar #'lsp--client-server-id
                        (lsp--filter-clients
                         (lambda (c)
                           (and (lsp--supports-buffer? c)
                                (lsp--server-binary-present? c)))))
                (user-error "No available LSP clients for %S" major-mode))))))
  (require 'lsp-mode)
  (let* ((client-sym (if (symbolp client) client (intern client)))
         (match (car (lsp--filter-clients
                      (lambda (c) (eq (lsp--client-server-id c) client-sym)))))
         (workspaces (lsp-workspaces)))
    (unless match
      (user-error "Couldn't find an LSP client named %S" client))
    (let ((old-priority (lsp--client-priority match)))
      (setf (lsp--client-priority match) 9999)
      (unwind-protect
          (if workspaces
              (lsp-workspace-restart
               (if (cdr workspaces)
                   (completing-read
                    "Select LSP workspace: "
                    (mapcar #'lsp--workspace-print workspaces)
                    nil t)
                 (car workspaces)))
            (lsp-mode +1))
        ;; Restore priority after initialization
        (add-hook
         'lsp-after-initialize-hook
         (lambda ()
           (setf (lsp--client-priority match) old-priority))
         nil 'local)))))
;; lsp
(defvar +lsp--default-read-process-output-max nil)
(defvar +lsp--default-gcmh-high-cons-threshold gc-cons-threshold)
(defvar +lsp--optimization-init-p nil)

(define-minor-mode lsp-optimization-mode
  "Deploys universal GC and IPC optimizations for `lsp-mode' and `eglot'."
  :global t
  :init-value nil
  (if (not lsp-optimization-mode)
      (setq-default read-process-output-max +lsp--default-read-process-output-max
                    gcmh-high-cons-threshold +lsp--default-gcmh-high-cons-threshold
                    +lsp--optimization-init-p nil)
    ;; Only apply these settings once!
    (unless +lsp--optimization-init-p
      (setq +lsp--default-read-process-output-max (default-value 'read-process-output-max)
            +lsp--default-gcmh-high-cons-threshold (default-value 'gcmh-high-cons-threshold))
      (setq-default read-process-output-max (* 2(* 1024 1024)))
      ;; REVIEW LSP causes a lot of allocations, with or without the native JSON
      ;;        library, so we up the GC threshold to stave off GC-induced
      ;;        slowdowns/freezes. Doom uses `gcmh' to enforce its GC strategy,
      ;;        so we modify its variables rather than `gc-cons-threshold'
      ;;        directly.
      (setq-default gcmh-high-cons-threshold (* 2 +lsp--default-gcmh-high-cons-threshold))
      (when (bound-and-true-p gcmh-mode)
        (gcmh-set-high-threshold))
      (setq +lsp--optimization-init-p t))))

(use-package lsp-mode
  ;; :ensure (:host github :repo "samwdp/lsp-mode" :branch "ols-server-download-url-malformed")
  :ensure (:host github :repo "emacs-lsp/lsp-mode")
  :after (evil)
  :demand t
  :hook ((lsp-mode . lsp-optimization-mode)
         (lsp-completion-mode . my/lsp-mode-setup-completion))
  :commands lsp-deferred
  :custom
  (read-process-output-max (* 3(* 1024 1024)))
  (lsp-completion-provider :none)
  :init
  (setq lsp-keymap-prefic "C-c")
  (setq lsp-diagnostics-provider :flycheck)
  (setq lsp-lens-enable nil
        lsp-signature-auto-activate nil
        lsp-signature-function 'lsp-signature-posframe)
  (setq lsp-headerline-breadcrumb-enable nil)
  ;; (setq lsp-copilot-enabled t)
  (defun my/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(flex))) ;; Configure flex
  :config
  (defun lsp-booster--advice-json-parse (old-fn &rest args)
    "Try to parse bytecode instead of json."
    (or
     (when (equal (following-char) ?#)
       (let ((bytecode (read (current-buffer))))
         (when (byte-code-function-p bytecode)
           (funcall bytecode))))
     (apply old-fn args)))
  (advice-add (if (progn (require 'json)
                         (fboundp 'json-parse-buffer))
                  'json-parse-buffer
                'json-read)
              :around
              #'lsp-booster--advice-json-parse)

  (defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
    "Prepend emacs-lsp-booster command to lsp CMD."
    (let ((orig-result (funcall old-fn cmd test?)))
      (if (and (not test?)                             ;; for check lsp-server-present?
               (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
               lsp-use-plists
               (not (functionp 'json-rpc-connection))  ;; native json-rpc
               (executable-find "emacs-lsp-booster"))
          (progn
            (when-let* ((command-from-exec-path (executable-find (car orig-result))))  ;; resolve command from exec-path (in case not found in $PATH)
              (setcar orig-result command-from-exec-path))
            (message "Using emacs-lsp-booster for %s!" orig-result)
            (cons "emacs-lsp-booster" orig-result))
        orig-result)))
  (advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command)
  (setq lsp-signature-render-documentation t)
  (define-key lsp-mode-map [remap xref-find-apropos] #'consult-lsp-symbols)
  (define-key lsp-mode-map [remap lookup-implementation] #'lsp-goto-implementation)
  (define-key lsp-mode-map [remap lookup-declaration] #'lsp-find-declaration)
  ;; (define-key lsp-mode-map [remap lookup-reference] #'lsp-find-references)
  (define-key lsp-mode-map [remap lookup-definition] #'lsp-find-definition)
  (define-key lsp-mode-map [remap lookup-type-definition] #'lsp-goto-type-definition)
  (evil-define-key 'normal lsp-mode-map (kbd "SPC c a") 'lsp-execute-code-action)
  (evil-global-set-key 'normal (kbd "g d") 'lsp-find-definition)
  (evil-global-set-key 'normal (kbd "g i") 'lsp-goto-implementation)
  (evil-global-set-key 'normal (kbd "g r") nil)
  (evil-global-set-key 'normal (kbd "g r r") 'lsp-find-references)
  (evil-global-set-key 'normal (kbd "g t") 'lsp-goto-type-definition)
  (evil-global-set-key 'normal (kbd "C-SPC") 'lsp-execute-code-action)
  (advice-add 'lsp-completion-at-point :around #'cape-wrap-buster)
  (advice-add 'lsp-completion-at-point :around #'cape-wrap-noninterruptible)
  )

(use-package web-mode
  :ensure (web-mode :host github :repo "fxbois/web-mode")
  :config
  (add-to-list 'auto-mode-alist '("\\.cshtml?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.razor?\\'" . web-mode))
  )
(use-package ccls
  :ensure (:host github :repo "emacs-lsp/emacs-ccls")
  :hook ((c-ts-mode c++-ts-mode) .
         (lambda () (require 'ccls) (lsp))))

;; (use-package lsp-tailwindcss
;;   :after (lsp)
;;   :init (setq lsp-tailwindcss-add-on-mode t)
;;   :ensure (lsp-tailwindcss :host github :repo "merrickluo/lsp-tailwindcss")
;;   :config
;;   (add-to-list 'lsp-tailwindcss-major-modes '(typescript-ts-mode css-ts-mode))
;;   )

(use-package lsp-ui
  :ensure t
  :hook ((lsp-mode . lsp-ui-mode))
  :init
  ;; (evil-define-key 'normal lsp-ui-mode-map (kbd "K") 'lsp-ui-doc-glance)
  (evil-define-key 'normal lsp-ui-mode-map (kbd "TAB") 'lsp-ui-doc-focus-frame)
  (evil-define-key 'normal lsp-ui-doc-frame-mode-map (kbd "<escape>") 'lsp-ui-doc-hide)
  (evil-define-key 'normal lsp-ui-doc-frame-mode-map (kbd "q") 'lsp-ui-doc-hide)
  :config
  (setq lsp-ui-doc-enable t
        lsp-ui-peek-enable t
        lsp-ui-doc-position 'at-point
        lsp-ui-doc-show-with-mouse nil
        lsp-ui-sideline-ignore-duplicate t
        lsp-ui-sideline-show-hover nil
        lsp-ui-sideline-actions-icon lsp-ui-sideline-actions-icon-default)
  (define-key lsp-mode-map [remap evil-lookup] #'lsp-ui-doc-glance)
  (evil-global-set-key 'normal (kbd "K") 'lsp-ui-doc-glance)

  (define-key lsp-ui-peek-mode-map (kbd "j") #'lsp-ui-peek--select-next)
  (define-key lsp-ui-peek-mode-map (kbd "k") #'lsp-ui-peek--select-prev)
  (define-key lsp-ui-peek-mode-map (kbd "M-j") #'lsp-ui-peek--select-next-file)
  (define-key lsp-ui-peek-mode-map (kbd "M-j") #'lsp-ui-peek--select-prev-file))

(use-package consult-lsp
  :ensure t)

(use-package lsp-treemacs-nerd-icons
  :after nerd-icons
  :ensure (:host github :repo "Velnbur/lsp-treemacs-nerd-icons")
  :init (with-eval-after-load 'lsp-treemacs
          (require 'lsp-treemacs-nerd-icons))
  )

(use-package lsp-treemacs
  :ensure t
  :custom (lsp-treemacs-theme "nerd-icons-ext"))


(use-package flycheck
  :ensure t
  :hook (lsp-mode . flycheck-mode)
  :bind (:map flycheck-mode-map
              ("C-n" . flycheck-next-error)
              ("C-p" . flycheck-previous-error))
  :custom
  (flycheck-display-errors-delay .3)
  (flycheck-checker-error-threshold 2000)
  )

(use-package consult-flycheck
  :ensure t)

(use-package format-all
  :ensure (format-all :fetcher github :repo "samwdp/emacs-format-all-the-code"))

(use-package yasnippet
  :ensure t
  :init (yas-global-mode))

(use-package yasnippet-capf
  :after cape
  :ensure (yasnippet-capf :fetcher github :repo "elken/yasnippet-capf")
  :config
  (add-to-list 'completion-at-point-functions #'yasnippet-capf)
  )

(use-package consult-yasnippet :ensure t)

(use-package yasnippet-snippets :ensure t)

(use-package competitive-programming-snippets :ensure t)

(use-package dap-mode
  :ensure t
  :commands dap-debug
  :hook (dap-mode . dap-tooltip-mode)
  :config

  (defvar my/golden-ratio-was-on t
    "Remember whether `golden-ratio-mode' was on before starting DAP.")

  (defun my/dap-disable-golden-ratio (&rest _)
    "Disable `golden-ratio-mode' when DAP session starts."
    (setq my/golden-ratio-was-on golden-ratio-mode)
    (when golden-ratio-mode
      (golden-ratio-mode -1)))

  (defun my/dap-restore-golden-ratio (&rest _)
    "Re-enable `golden-ratio-mode' if it was on before DAP."
    (when my/golden-ratio-was-on
      (golden-ratio-mode +1)))

  ;; Hook into DAP session start/end
  (with-eval-after-load 'dap-mode
    (add-hook 'dap-session-created-hook    #'my/dap-disable-golden-ratio)
    (add-hook 'dap-terminated-hook         #'my/dap-restore-golden-ratio)
    (add-hook 'dap-exited-hook             #'my/dap-restore-golden-ratio))
  (require 'dap-node)
  (require 'dap-chrome)
  (require 'dap-firefox)
  (require 'dap-edge)
  (require 'dap-netcore)
  (require 'dap-lldb)
  (require 'dap-cpptools))

(use-package cond-let
  :ensure (cond-let :host github :repo "tarsius/cond-let"))

(use-package magit
  :ensure t :demand t
  :config
  (when IS-WINDOWS
    (setq magit-git-executable "C:/Program Files/Git/mingw64/bin/git.exe")
    )
  (setq git-commit-major-mode 'git-commit-ts-mode))

(use-package dired
  :ensure nil
  :config
  (setq ls-lisp-dirs-first t)
  (setq dired-dwim-target t))

(use-package dirvish
  :ensure t :demand t
  :config
  (dirvish-override-dired-mode)
  (evil-define-key 'normal dired-mode-map (kbd "o") 'dired-create-empty-file)
  (evil-collection-define-key 'normal 'dired-mode-map (kbd "SPC") nil)
  (setq dirvish-attributes
        (append
         ;; The order of these attributes is insignificant, they are always
         ;; displayed in the same position.
         '(vc-state subtree-state nerd-icons collapse)
         ;; Other attributes are displayed in the order they appear in this list.
         '(git-msg file-size))
        dirvish-hide-details t))

(use-package diredfl
  :ensure t :demand t
  :hook
  ((dired-mode . diredfl-mode)
   ;; highlight parent and directory preview as well
   (dirvish-directory-view-mode . diredfl-mode))
  :config
  (set-face-attribute 'diredfl-dir-name nil :bold t))

(use-package treesit
  :ensure nil :demand t
  :config
  (add-to-list 'treesit-language-source-alist '(odin "https://github.com/tree-sitter-grammars/tree-sitter-odin"))
  (add-to-list 'treesit-language-source-alist '(powershell "https://github.com/airbus-cert/tree-sitter-powershell" "v0.24"))
  (add-to-list 'treesit-language-source-alist '(markdown "https://github.com/tree-sitter-grammars/tree-sitter-markdown" "v0.5.0" "tree-sitter-markdown/src"))
  (add-to-list 'treesit-language-source-alist '(markdown-inline "https://github.com/tree-sitter-grammars/tree-sitter-markdown" "v0.5.0" "tree-sitter-markdown-inline/src"))
  (add-to-list 'treesit-language-source-alist '(gitcommit "https://github.com/gbprod/tree-sitter-gitcommit" "v0.3.3" "src")))

(use-package treesit-context-overlay
  :ensure (treesit-context-overlay :host github :repo "samwdp/treesit-context-overlay")
  :config
  (setq treesit-context-overlay-face "#bdae93"
        treesit-context-overlay-delimiter "=>"))

(use-package treesit-context-headerline
  :ensure (treesit-context-headerline :host github :repo "samwdp/treesit-context-headerline")
  :config
  (setq treesit-context-headerline-separator '("nf-cod-chevron_right" . nerd-icons)))

(use-package evil-markdown
  :after (markdown-ts-mode)
  :hook (markdown-ts-mode . evil-markdown-mode)
  :ensure (evil-markdown :host github :repo "samwdp/evil-markdown"))

(use-package evil-numbers
  :after evil
  :ensure t
  :config
  (evil-global-set-key 'normal (kbd "g C-a") 'evil-numbers/inc-at-pt-incremental)
  (evil-global-set-key 'normal (kbd "g C-x") 'evil-numbers/dec-at-pt-incremental)
  (evil-global-set-key 'visual (kbd "g C-a") 'evil-numbers/inc-at-pt-incremental)
  (evil-global-set-key 'visual (kbd "g C-x") 'evil-numbers/dec-at-pt-incremental))

(use-package grip-mode :ensure t)
(use-package ox-gfm :ensure t)

(use-package harpoon
  :ensure t :demand t
  :config
  (setq harpoon-project-package 'project))

(use-package yaml-ts-mode
  :ensure nil
  :hook (
         (yaml-ts-mode . lsp-deferred)
         (yaml-ts-mode . treesit-context-overlay-mode)
         (yaml-ts-mode . treesit-context-headerline-mode))
  :mode (("\\.yaml\\'" . yaml-ts-mode)))

(use-package json-ts-mode
  :ensure nil
  :hook (

         (json-ts-mode . lsp-deferred)
         (json-ts-mode . treesit-context-overlay-mode)
         (json-ts-mode . treesit-context-headerline-mode))
  :mode (("\\.json\\'" . json-ts-mode)))

(use-package typescript-ts-mode
  :ensure nil
  :hook (
         (tsx-ts-mode . lsp-deferred)
         (tsx-ts-mode . treesit-context-overlay-mode)
         (tsx-ts-mode . treesit-context-headerline-mode)
         (typescript-ts-mode . lsp-deferred)
         (typescript-ts-mode . treesit-context-overlay-mode)
         (typescript-ts-mode . treesit-context-headerline-mode))
  :mode (("\\.ts\\'" . typescript-ts-mode)
         ("\\.tsx\\'" . tsx-ts-mode)))

(use-package rust-ts-mode
  :ensure nil
  :hook (
         (rust-ts-mode . lsp-deferred)
         (rust-ts-mode . treesit-context-overlay-mode)
         (rust-ts-mode . treesit-context-headerline-mode))
  :mode ("\\.rs\\'" . rust-ts-mode))

(use-package html-ts-mode
  :ensure nil
  :hook (
         (html-ts-mode . lsp-deferred)
         (html-ts-mode . treesit-context-overlay-mode)
         (html-ts-mode . treesit-context-headerline-mode))
  :mode ("\\.html\\'" . html-ts-mode))

(use-package go-ts-mode
  :ensure nil
  :hook (
         (go-ts-mode . lsp-deferred)
         (go-ts-mode . treesit-context-overlay-mode)
         (go-ts-mode . treesit-context-headerline-mode))
  )


(use-package csharp-ts-mode
  :ensure nil
  :hook (
         (csharp-ts-mode . lsp-deferred)
         (csharp-ts-mode . treesit-context-overlay-mode)
         (csharp-ts-mode . treesit-context-headerline-mode))
  :mode ("\\.cs\\'" . csharp-ts-mode))

(use-package css-ts-mode
  :ensure nil
  :hook (
         (css-ts-mode . lsp-deferred)
         (css-ts-mode . treesit-context-overlay-mode)
         (css-ts-mode . treesit-context-headerline-mode)
         (scss-ts-mode . lsp-deferred)
         (scss-ts-mode . treesit-context-overlay-mode)
         (scss-ts-mode . treesit-context-headerline-mode))
  :mode (("\\.css\\'" . css-ts-mode)
         ("\\.scss\\'" . css-ts-mode)))

(use-package odin-ts-mode
  :ensure (odin-ts-mode :host github :repo "Sampie159/odin-ts-mode")
  :hook (
         (odin-ts-mode . lsp-deferred)
         (odin-ts-mode . treesit-context-overlay-mode)
         (odin-ts-mode . treesit-context-headerline-mode))
  :mode ("\\.odin\\'" . odin-ts-mode))

(use-package markdown-ts-mode
  :ensure t :demand t
  :mode ("\\.md\\'" . markdown-ts-mode))

(use-package git-commit-ts-mode
  :ensure t :demand t
  :mode "\\COMMIT_EDITMSG\\'")

(use-package powershell-ts-mode
  :ensure (:host github :repo "dmille56/powershell-ts-mode")
  :hook (

         (powershell-ts-mode . treesit-context-overlay-mode)
         (powershell-ts-mode . treesit-context-headerline-mode))
  :config
  ;; Optional: if you want to disable top-level vars from being shown in imenu
  (setq powershell-ts-enable-imenu-top-level-vars nil))

(use-package eca
  :ensure (eca :host github :repo "editor-code-assistant/eca-emacs")
  )

(use-package copilot
  :ensure (:host github :repo "copilot-emacs/copilot.el" :files ("*.el"))
  :config
  (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
  (define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)
  (define-key copilot-completion-map (kbd "M-n") 'copilot-next-completion)
  (define-key copilot-completion-map (kbd "M-p") 'copilot-previous-completion)
  )

(use-package eshell
  :ensure nil
  :config
  (add-hook 'eshell-mode-hook (lambda () (setenv "TERM" "xterm-256color")))
  )

(use-package eshell-z :ensure t)
(use-package eshell-syntax-highlighting
  :ensure t
  :hook (eshell-mode . eshell-syntax-highlighting-mode))

(use-package eshell-did-you-mean
  :ensure t
  :config
  (eshell-did-you-mean-setup))

(use-package org
  :ensure t :demand t
  :after evil
  :config
  (when IS-WINDOWS
    (setq org-directory "c:/Users/sam/Documents/org")
    )
  (evil-define-key 'normal org-mode-map (kbd "C-j") 'windmove-down)
  (evil-define-key 'normal org-mode-map (kbd "C-k") 'windmove-up)
  (evil-define-key 'normal org-mode-map (kbd "C-h") 'windmove-left)
  (evil-define-key 'normal org-mode-map (kbd "C-l") 'windmove-right)
  ;; (setq org-export-with-broken-links t)
  )

(use-package toc-org
  :ensure t
  :hook (org-mode . toc-org-mode)
  )

(use-package org-modern
  :ensure t
  :hook((org-mode . org-modern-mode)
        (org-agenda-finilize . org-modern-agenda))
  :config
  (setq org-modern-star 'replace))

(use-package org-appear
  :ensure (org-appear :type git :fetcher github :repo "awth13/org-appear")
  :hook (org-mode . org-appear-mode)
  :config
  (setq org-appear-autolinks t
        org-appear-autoemphasis t
        org-appear-autoentities t
        org-appear-autokeywords t
        org-appear-autosubmarkers t))


(use-package org-fancy-priorities
  :ensure t
  :hook ((org-mode org-agenda-mode) . org-fancy-priorities-mode))

(use-package evil-org
  :ensure t
  :after evil
  :hook (org-mode . evil-org-mode))

(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory (file-truename "C:/Users/sam/Documents/org/roam/"))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today))
  :config
  ;; If you're using a vertical completion framework, you might want a more informative completion interface
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  ;; If using org-roam-protocol
  (require 'org-roam-protocol))

(use-package org-roam-ui
  :ensure
    (:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
    :after org-roam
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t))


(use-package package-lint
  :ensure t)

(use-package flycheck-package
  :ensure t)

(use-package vc-jj
  :ensure (:repo "https://codeberg.org/emacs-jj-vc/vc-jj.el" :files ("*.el")))

(use-package jj-mode
  :ensure (:host github :repo "bolivier/jj-mode.el")
  :config
  (when (featurep 'evil)
    (evil-define-key 'normal jj-mode-map (kbd ".") 'jj-goto-current)
    (evil-define-key 'normal jj-mode-map (kbd "g") 'jj-log-refresh)
    (evil-define-key 'normal jj-mode-map (kbd "c") 'jj-commit)
    (evil-define-key 'normal jj-mode-map (kbd "d") 'jj-describe)
    (evil-define-key 'normal jj-mode-map (kbd "e") 'jj-edit-changeset)
    (evil-define-key 'normal jj-mode-map (kbd "u") 'jj-undo)
    (evil-define-key 'normal jj-mode-map (kbd "s") 'jj-squash-transient)
    (evil-define-key 'normal jj-mode-map (kbd "N") 'jj-new)
    (evil-define-key 'normal jj-mode-map (kbd "r") 'jj-rebase-transient)
    (evil-define-key 'normal jj-mode-map (kbd "b") 'jj-bookmark-transient)
    (evil-define-key 'normal jj-mode-map (kbd "G") 'jj-git-transient)
    ))

(use-package buffer-terminator
  :ensure t
  :custom
  (buffer-terminator-verbose nil)
  :config
  (buffer-terminator-mode 1))

(use-package project-discover
  :ensure (project-discover :host github :repo "samwdp/project-discover")
  :after project
  :hook (kill-emacs . project-discover-run)
  :config
  (setq project-discover-directory-alist '(("c:/Users/sam" . 3)
                                           ("w:/"          . 5)
                                           ("p:/"          . 4))))

(use-package lsp-bridge
  :ensure '(lsp-bridge :type git :host github :repo "samwdp/lsp-bridge"
                       :files (:defaults "*.el" "*.py" "acm" "core" "langserver" "multiserver" "resources")
                       :build (:not compile))
  :bind (:map acm-mode-map
              ("C-n" . acm-select-next)
              ("C-p" . acm-select-prev)
              ("C-y" . acm-complete)
              ("<return>" . nil)
              ("RET" . nil)
              ("TAB" . nil)
              ("<tab>" . nil)
              )
  :init
  ;; (global-lsp-bridge-mode)
  :config
  (with-eval-after-load 'acm
    (evil-define-key 'insert acm-mode-map (kbd "C-y") #'acm-complete)
    (evil-define-key 'insert acm-mode-map (kbd "C-n") #'acm-select-next)
    (evil-define-key 'insert acm-mode-map (kbd "C-p") #'acm-select-prev)
    (define-key acm-mode-map (kbd "C-y") #'acm-complete)
    (define-key acm-mode-map (kbd "C-n") #'acm-select-next)
    (define-key acm-mode-map (kbd "C-p") #'acm-select-prev))
  (setq acm-enable-tabnine nil
        acm-enable-copilot t
        lsp-bridge-csharp-lsp-server "csharp-ls"
        ;; lsp-bridge-csharp-lsp-server "omnisharp-dotnet"
        lsp-bridge-enable-inlay-hint t
        lsp-bridge-enable-diagnostics t
        lsp-bridge-enable-hover-diagnostic t
        )
  (add-to-list 'lsp-bridge-default-mode-hooks 'odin-ts-mode-hook)
  (add-to-list 'lsp-bridge-default-mode-hooks 'html-ts-mode-hook)
  (evil-global-set-key 'normal (kbd "K") 'lsp-bridge-popup-documentation)
  (evil-global-set-key 'normal (kbd "g d") 'lsp-bridge-find-def)
  (evil-global-set-key 'normal (kbd "g i") 'lsp-bridge-find-impl)
  (evil-global-set-key 'normal (kbd "g r") nil)
  (evil-global-set-key 'normal (kbd "g r r") 'lsp-bridge-find-references)
  (evil-global-set-key 'normal (kbd "g t") 'lsp-bridge-find-type-def)
  )

(use-package dotenv-mode
  :ensure (dotenv-mode :host github :repo "preetpalS/emacs-dotenv-mode")
  :config
  (add-to-list 'auto-mode-alist '("\\.env\\..*\\'" . dotenv-mode)) ;; for optionally supporting additional file extensions such as `.env.test' with this major mode
  )

(use-package project-compile-detector
  :ensure (project-compile-detector :host github :repo "samwdp/project-compile-detector.el")
  :bind
  (("C-c c" . project-compile-detector-compile-project)
   ("C-c r" . project-compile-detector-recompile))
  :custom
  (project-compile-detector-log-level 'debug)
  (project-compile-detector-smart-suggest t)
  :config
  (project-compile-detector-setup))

(use-package wgrep
  :after evil-collection
  :ensure t
  :demand t)

(use-package popper
  :ensure (popper :host github :repo "karthink/popper")
  :hook (persp-mode . popper-mode)
  :bind (("C-`"   . popper-toggle)
         ("C-S-n" . popper-cycle)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :init
  (setq popper-group-function #'popper-group-by-perspective)
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          "\\*Embark Export\\*"
          "^\\*Copilot"
          "^<eca-chat:[0-9:]\\+>$" eca-chat-mode
          "^\\*.*eshell.*\\*$" eshell-mode ;eshell as a popup
          "^\\*.*shell.*\\*$"  shell-mode  ;shell as a popup
          "^\\*.*term.*\\*$"   term-mode   ;term as a popup
          "^\\*.*vterm.*\\*$"  vterm-mode  ;vterm as a popup
          "^\\*.*eat.*\\*$"  eat-mode  ;vterm as a popup
          "^\\*.*jj-log:.*\\*$"  jj-mode  ;vterm as a popup
          grep-mode
          help-mode
          magit-status-mode
          "COMMIT_EDITMSG"                       ;; exact match
          git-commit-ts-mode
          compilation-mode)))

(use-package big-font-mode
  :load-path "./modules/big-font-mode"
  )
