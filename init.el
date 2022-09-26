(setq gc-cons-thershold 100000000)
(setq read-process-output-max (* 1024 1024)) ;; 1mb

(defun sp/defer-garbage-collection-h ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun sp/restore-garbage-collection-h ()
  ;; Defer it so that commands launched immediately after will enjoy the
  ;; benefits.
  (run-at-time
   1 nil (lambda () (setq gc-cons-threshold 16777216))))

(add-hook 'minibuffer-setup-hook #'sp/defer-garbage-collection-h)
(add-hook 'minibuffer-exit-hook #'sp/restore-garbage-collection-h)

(setq emacs-version-short  (replace-regexp-in-string
                            "\\([0-9]+\\)\\.\\([0-9]+\\).*"
                            "\\1_\\2" emacs-version))

(setq custom-file (expand-file-name
                   (concat "custom_" emacs-version-short ".el")
                   user-emacs-directory))

(defconst NATIVECOMP (if (fboundp 'native-comp-available-p) (native-comp-available-p)))
(defconst IS-MAC     (eq system-type 'darwin))
(defconst IS-LINUX   (eq system-type 'gnu/linux))
(defconst IS-WINDOWS (memq system-type '(cygwin windows-nt ms-dos)))

(add-to-list 'load-path "~/.emacs.d/custom/")

;; (server-start)
(defun efs/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                    (time-subtract after-init-time before-init-time)))
           gcs-done))

(when NATIVECOMP
  (setq native-comp-async-report-warnings-errors nil)
  (add-to-list 'native-comp-eln-load-path (expand-file-name "eln-cache/" "~/.emacs.cache")))

(unless (featurep 'straight)
  ;; Bootstrap straight.el
  (defvar bootstrap-version)
  (let ((bootstrap-file
         (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
        (bootstrap-version 5))
    (unless (file-exists-p bootstrap-file)
      (with-current-buffer
          (url-retrieve-synchronously
           "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
           'silent 'inhibit-cookies)
        (goto-char (point-max))
        (eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage)))

;; Use straight.el for use-package expressions
(straight-use-package 'use-package)

(use-package straight
  :custom (straight-use-package-by-default t))

(use-package gcmh
  :config
  (gcmh-mode 1))

(use-package unicode-fonts
  :straight (unicode-fonts :type git :host github :repo "yurikhan/unicode-fonts" :branch "fix-daemon-startup"))

(use-package fancy-battery
  :hook (after-init . fancy-battery-mode))

(blink-cursor-mode 0)
(pixel-scroll-mode 1)
(display-time-mode 1)
(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room
(global-visual-line-mode)
(menu-bar-mode -1)            ; Disable the menu bar
(setq scroll-margin 8
      visible-bell t
      create-lockfiles nil
      use-short-answers t
      ring-bell-function nil
      make-backup-files t
      backup-by-copying t
      inhibit-startup-message t
      inhibit-compacting-font-caches t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t
      set-language-environment "UTF-8")

(defvar sp/text-height 17)
;; (defvar sp/text-height 28)
(defvar sp/text-height-variable 20)
(defvar sp/font-string "LiterationMono Nerd Font")

(defun sp/new-frame ()
  (set-face-attribute 'default nil :font (font-spec :family sp/font-string :size sp/text-height))
  (set-face-attribute 'fixed-pitch nil :font (font-spec :family sp/font-string :size sp/text-height))
  (set-face-attribute 'fixed-pitch-serif nil :font (font-spec :family sp/font-string :size sp/text-height))
  (set-face-attribute 'variable-pitch nil :font (font-spec :family sp/font-string :size sp/text-height-variable))
  (toggle-frame-maximized))

(defun unicode-fonts-setup-h (frame)
  "Run unicode-fonts-setup, then remove the hook."
  (when (and frame (display-graphic-p frame))
    (with-selected-frame frame
      (require 'unicode-fonts)
      (sp/new-frame)
      (unicode-fonts-setup))))

(if (display-graphic-p)
    (unicode-fonts-setup-h (selected-frame))
  (add-hook 'after-make-frame-functions 'unicode-fonts-setup-h))

(setq-default indent-tabs-mode nil
              tab-width 4)
(setq-default tab-always-indent nil)

(defconst emacs-tmp-dir (expand-file-name "temp/" "~/.emacs.cache"))
(setq backup-directory-alist
      `((".*" . emacs-tmp-dir)))

(global-set-key (kbd "C-h") 'windmove-left)
(global-set-key (kbd "C-j") 'windmove-down)
(global-set-key (kbd "C-k") 'windmove-up)
(global-set-key (kbd "C-l") 'windmove-right)
(global-set-key (kbd "C-SPC") 'company-complete)
(global-set-key (kbd "C-.") 'lsp-execute-code-action)

(use-package no-littering
  :config
  (setq auto-save-file-name-transforms
	    `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
  )

(setq custom-theme-directory (concat user-emacs-directory "themes/"))
(use-package doom-themes
  :init (load-theme 'gruvbox t))

(use-package doom-modeline
  :hook (doom-modeline-mode . size-indication-mode)
  :hook (doom-modeline-mode . column-number-mode)
  :config
  (setq doom-modeline-bar-width 3
        doom-modeline-github nil
        doom-modeline-mu4e nil
        doom-modeline-persp-name t
        doom-modeline-workspace-name nil
        doom-modeline-minor-modes nil
        doom-modeline-major-mode-icon nil
        doom-modeline-vcs-max-length 50
        doom-modeline-buffer-file-name-style 'truncate-all
        ;; Only show file encoding if it's non-UTF-8 and different line endings
        ;; than the current OSes preference
        doom-modeline-buffer-encoding 'nondefault
        doom-modeline-default-eol-type
        (cond (IS-MAC 2)
              (IS-WINDOWS 1)
              (0)))
  :init
  (doom-modeline-mode))

(use-package which-key
  :defer 0
  :diminish which-key-mode
  :init (which-key-mode)
  :config
  (setq which-key-idle-delay 0.3))

(use-package meow
  :config
  (setq meow-use-clipboard t)
  (setq meow-cheatsheet-physical-layout meow-cheatsheet-physical-layout-iso)
  (defun meow-setup ()
    (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
    (meow-motion-overwrite-define-key
     '("j" . meow-next)
     '("k" . meow-prev)
     '("<escape>" . ignore))
    (meow-leader-define-key
     ;; SPC j/k will run the original command in MOTION state.
     '("j" . "H-j")
     '("k" . "H-k")
     ;; Use SPC (0-9) for digit arguments.
     '("1" . meow-digit-argument)
     '("2" . meow-digit-argument)
     '("3" . meow-digit-argument)
     '("4" . meow-digit-argument)
     '("5" . meow-digit-argument)
     '("6" . meow-digit-argument)
     '("7" . meow-digit-argument)
     '("8" . meow-digit-argument)
     '("9" . meow-digit-argument)
     '("0" . meow-digit-argument)
     '("." . find-file)
     '("/" . meow-keypad-describe-key)
     '("?" . meow-cheatsheet)
     '("SPC" . consult-projectile-find-file)
     '("TAB" . persp-switch)
     '("ac" . quick-calc)
     '("bb" . consult-projectile-switch-to-buffer)
     '("bd" . kill-this-buffer)
     '("bB" . consult-buffer)
     '("cc" . projectile-compile-project)
     '("og" . magit-status)
     '("pp" . projectile-switch-project)
     '("op" . +treemacs/toggle)
     '("fs" . write-file)
     '("fde" . (lambda ()
                 (interactive)
                 (find-file (expand-file-name (concat user-emacs-directory "init.el")))))
     '("sp" . consult-ripgrep)
     '("ss" . consult-line)
     )
    (meow-normal-define-key
     '("0" . meow-expand-0)
     '("9" . meow-expand-9)
     '("8" . meow-expand-8)
     '("7" . meow-expand-7)
     '("6" . meow-expand-6)
     '("5" . meow-expand-5)
     '("4" . meow-expand-4)
     '("3" . meow-expand-3)
     '("2" . meow-expand-2)
     '("1" . meow-expand-1)
     '("-" . negative-argument)
     '(";" . meow-reverse)
     '("," . meow-inner-of-thing)
     '("." . meow-bounds-of-thing)
     '("#" . comment-line)
     '("/" . meow-visit)
     '("?" . meow-comment)
     '("[" . meow-beginning-of-thing)
     '("]" . meow-end-of-thing)
     '("a" . meow-append)
     '("A" . meow-open-below)
     '("b" . meow-back-word)
     '("B" . meow-back-symbol)
     '("c" . meow-change)
     '("d" . meow-delete)
     '("D" . meow-backward-delete)
     '("w" . meow-next-word)
     '("W" . meow-next-symbol)
     '("f" . meow-find)
     '("g" . meow-cancel-selection)
     '("G" . meow-grab)
     '("h" . meow-left)
     '("H" . meow-left-expand)
     '("i" . meow-insert)
     '("I" . meow-open-above)
     '("j" . meow-next)
     '("J" . meow-next-expand)
     '("k" . meow-prev)
     '("K" . meow-prev-expand)
     '("l" . meow-right)
     '("L" . meow-right-expand)
     '("m" . meow-join)
     '("n" . meow-search)
     '("o" . meow-block)
     '("O" . meow-to-block)
     '("p" . yank)
     '("q" . meow-quit)
     '("Q" . meow-goto-line)
     '("r" . meow-replace)
     '("R" . meow-swap-grab)
     '("s" . meow-kill)
     '("t" . meow-till)
     '("u" . meow-undo)
     '("U" . meow-undo-in-selection)
     '("v" . meow-visit)
     '("e" . meow-mark-word)
     '("E" . meow-mark-symbol)
     '("x" . meow-line)
     '("X" . meow-goto-line)
     '("y" . meow-save)
     '("Y" . meow-sync-grab)
     '("z" . meow-pop-selection)
     '("'" . repeat)
     '("<escape>" . ignore)))
  (meow-setup)
  :init
  (meow-global-mode 1))

(use-package multiple-cursors)

(use-package all-the-icons)

(use-package all-the-icons-completion
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init
  (all-the-icons-completion-mode))

(use-package all-the-icons-dired
  :defer t)

(use-package smartparens
  :defer t
  :init
  (smartparens-global-mode))

(use-package anzu
  :init
  (global-anzu-mode +1))

(use-package marginalia
  :after vertico
  :custom
  (marginalia-max-relative-age 0)
  (marginalia-align 'right)
  :init
  (marginalia-mode))

(use-package beacon
  :defer t
  :init
  (beacon-mode 1))

(use-package dtrt-indent)

(use-package adaptive-wrap)

;; (use-package topsy
;;   :straight (topsy :type git :host github :repo "alphapapa/topsy.el" :branch "master")
;;   :hook (prog-mode . topsy-mode))

(require 'word-wrap)
(+global-word-wrap-mode 1)
(use-package drag-stuff
  :defer t
  :config
  (global-set-key (kbd "M-k") 'drag-stuff-up)
  (global-set-key (kbd "M-j") 'drag-stuff-down))


(use-package rainbow-mode
  :defer t
  :hook ((prog-mode . rainbow-mode)
         (org-mode . rainbow-mode)
         (fundamental-mode . rainbow-mode)))

(use-package perspective
  :custom
  (persp-mode-prefix-key (kbd "C-c C-p"))
  :config
  (add-hook 'persp-created-hook (lambda () (split-window-horizontally)))
  (setq persp-nil-name "main"
        persp-modestring-short t)
  (persp-mode))


(use-package prescient)

(use-package company
  :bind (:map company-active-map
              ("<tab>" . company-complete-selection))
  :init
  (global-company-mode 1)
  :config
  (add-to-list 'company-backends #'company-files)
  (setq company-minimum-prefix-length 2
        company-idle-delay 0.0
        company-tooltip-limit 50))

(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package company-shell
  :config (add-to-list 'company-backends #'company-shell))

(use-package company-tabnine)

(use-package company-prescient)

(use-package vertico
  :config
  (setq vertico-resize nil
        vertico-count 17
        vertico-cycle t
        completion-in-region-function
        (lambda (&rest args)
          (apply (if vertico-mode
                     #'consult-completion-in-region
                   #'completion--in-region)
                 args)))
  :init
  (vertico-mode))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package vertico-posframe
  :after vertico
  :config
  (setq vertico-posframe-parameters
        '((left-fringe . 5)
          (right-fringe . 5)
          (min-height . ,vertico-count)))
  (setq vertico-posframe-border-width 2)
  (setq vertico-posframe-poshandler #'posframe-poshandler-frame-bottom-center)
  :init
  (vertico-posframe-mode 1))

(use-package embark
  :defer t)

(use-package consult
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)
  (advice-add #'register-preview :override #'consult-register-window)
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :config
  (consult-customize
   consult-theme
   :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-recent-file
   consult--source-project-recent-file
   :preview-key (kbd "M-."))
  (setq consult-narrow-key "<"))

(use-package embark-consult
  :after (embark consult))

(use-package consult-dir)

(use-package consult-flycheck)

(use-package consult-projectile
  :straight (consult-projectile :type git :host gitlab :repo "OlMon/consult-projectile" :branch "master"))

(use-package flycheck
  :commands flycheck-list-errors flycheck-buffer
  :config
  (setq flycheck-emacs-lisp-load-path 'inherit)
  (delq 'new-line flycheck-check-syntax-automatically)
  (setq flycheck-idle-change-delay 1.0)
  (setq flycheck-buffer-switch-check-intermediate-buffers t)
  (setq flycheck-display-errors-delay 0.25))

(use-package projectile
  :defer t
  :config
  (setq projectile-indexing-method 'hybrid)
  (define-key projectile-mode-map (kbd "C-x p") 'projectile-command-map)
  (projectile-mode +1)
  :init
  (setq projectile-enable-caching t)
  (setq projectile-project-search-path '("W:/"
                                         "W:/personal/angular/src"
                                         "W:/personal/c/src"
                                         "W:/personal/cpp/src"
                                         "W:/personal/csharp/src"
                                         "W:/personal/emacs/src"
                                         "W:/personal/odin/src"
                                         "W:/personal/go/src"
                                         "W:/personal/rust/src"
                                         "W:/personal/odin/src"
                                         "W:/foresolutions")))

(use-package persp-projectile)

(use-package marginalia
  :after vertico
  :ensure t
  :init
  (add-hook 'marginalia-mode-hook #'all-the-icons-completion-marginalia-setup)
  (marginalia-mode))

(use-package persp-projectile)

(use-package treemacs
  :defer t
  :init
  (setq treemacs-follow-after-init t
        treemacs-is-never-other-window nil
        treemacs-width 50
        treemacs-position 'right
        treemacs-sorting 'alphabetic-case-insensitive-asc)
  :config
  (treemacs-follow-mode -1))

(use-package treemacs-magit
  :after treemacs magit)

(use-package lsp-treemacs
  :after (treemacs lsp))

(use-package treemacs-persp ;;treemacs-perspective if you use perspective.el vs. persp-mode
  :after (treemacs persp-mode) ;;or perspective vs. persp-mode
  :ensure t
  :config (treemacs-set-scope-type 'Perspectives))

(defun +treemacs/toggle ()
  "Initialize or toggle treemacs.
Ensures that only the current project is present and all other projects have
been removed.
Use `treemacs' command for old functionality."
  (interactive)
  (require 'treemacs)
  (pcase (treemacs-current-visibility)
    (`visible (delete-window (treemacs-get-local-window)))
    (_ (if (sp/project-p)
           (treemacs-add-and-display-current-project)
         (treemacs)))))

(defun sp/project-p (&optional dir)
  "Return t if DIR (defaults to `default-directory') is a valid project."
  (and (sp/project-root dir)
       t))

(defun sp/project-root (&optional dir)
  "Return the project root of DIR (defaults to `default-directory').
Returns nil if not in a project."
  (let ((projectile-project-root
         (unless dir (bound-and-true-p projectile-project-root)))
        projectile-require-project-root)
    (projectile-project-root dir)))

(defvar +lsp--default-read-process-output-max nil)
(defvar +lsp--default-gcmh-high-cons-threshold nil)
(defvar +lsp--optimization-init-p nil)

(define-minor-mode +lsp-optimization-mode
  "Deploys universal GC and IPC optimizations for `lsp-mode' and `eglot'."
  :global t
  :init-value nil
  (if (not +lsp-optimization-mode)
      (setq-default read-process-output-max +lsp--default-read-process-output-max
                    gcmh-high-cons-threshold +lsp--default-gcmh-high-cons-threshold
                    +lsp--optimization-init-p nil)
    ;; Only apply these settings once!
    (unless +lsp--optimization-init-p
      (setq +lsp--default-read-process-output-max
            ;; DEPRECATED Remove check when 26 support is dropped
            (if (boundp 'read-process-output-max)
                (default-value 'read-process-output-max))
            +lsp--default-gcmh-high-cons-threshold
            (default-value 'gcmh-high-cons-threshold))
      ;; `read-process-output-max' is only available on recent development
      ;; builds of Emacs 27 and above.
      (setq-default read-process-output-max (* 1024 1024))
      ;; REVIEW LSP causes a lot of allocations, with or without Emacs 27+'s
      ;;        native JSON library, so we up the GC threshold to stave off
      ;;        GC-induced slowdowns/freezes. Doom uses `gcmh' to enforce its
      ;;        GC strategy, so we modify its variables rather than
      ;;        `gc-cons-threshold' directly.
      (setq-default gcmh-high-cons-threshold (* 2 +lsp--default-gcmh-high-cons-threshold))
      (gcmh-set-high-threshold)
      (setq +lsp--optimization-init-p t))))

(defvar +lsp-company-backends
  '(company-tabnine :separate company-capf company-yasnippet)
  "The backends to prepend to `company-backends' in `lsp-mode' buffers.
Can be a list of backends; accepts any value `company-backends' accepts.")

;; (require 'init-tabnine-capf)
(use-package lsp-mode
  :hook ((csharp-tree-sitter-mode
          web-mode
          odin-mode
          typescript-mode
          json-mode
          yaml-mode
          css-mode
          ;; csproj-mode
          sass-mode
          go-mode
          pwsh-mode
          cc-mode
          c-mode
          sql-mode
          rust-mode
          dockerfile-mode) . #'lsp)
  :hook (lsp-mode . +lsp-optimization-mode)
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l")
  (setq lsp-headerline-breadcrumb-enable t
        lsp-keep-workspace-alive nil
        lsp-modeline-diagnostics-enable nil
        lsp-modeline-code-actions-enable nil
        lsp-lens-enable nil
        lsp-enable-file-watchers nil
        lsp-enable-folding nil
        lsp-enable-text-document-color nil
        lsp-enable-on-type-formatting nil)
  :config

  (add-hook 'lsp-completion-mode-hook
            (defun +lsp-init-company-backends-h ()
              (when lsp-completion-mode
                (set (make-local-variable 'company-backends)
                     (cons +lsp-company-backends
                           (remove +lsp-company-backends
                                   (remq 'company-capf company-backends)))))))
  
  (define-key lsp-mode-map [remap xref-find-apropos] #'consult-lsp-symbols)

  (add-to-list 'lsp-language-id-configuration '(odin-mode . "odin"))
  (add-to-list 'lsp-language-id-configuration '(sql-mode . "sql"))
  ;; (add-to-list 'lsp-language-id-configuration '(csproj-mode . "csharp"))

  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection "ols")
                    :major-modes '(odin-mode)
                    :server-id 'ols
                    :multi-root t))
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection "c:/tools/sqls/sqls.exe")
                    :major-modes '(sql-mode)
                    :server-id 'sql
                    :multi-root t))

  (add-hook 'before-save-hook 'lsp-format-buffer)
  (lsp-enable-which-key-integration t))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-doc-max-height 8
        lsp-ui-doc-max-width 72
        lsp-ui-doc-show-with-cursor t
        lsp-ui-doc-show-with-mouse nil
        lsp-ui-sideline-show-hover nil
        lsp-ui-sideline-actions-icon lsp-ui-sideline-actions-icon-default
        lsp-ui-doc-include-signature t
        lsp-ui-doc-delay 0.75
        lsp-ui-sideline-ignore-duplicate t
        lsp-ui-doc-alignment 'window
        lsp-ui-doc-position 'at-point
        lsp-ui-doc-use-webkit t)
  (define-key lsp-ui-peek-mode-map (kbd "j") #'lsp-ui-peek--select-next)
  (define-key lsp-ui-peek-mode-map (kbd "k") #'lsp-ui-peek--select-prev)
  (define-key lsp-ui-peek-mode-map (kbd "M-j") #'lsp-ui-peek--select-next-file)
  (define-key lsp-ui-peek-mode-map (kbd "M-j") #'lsp-ui-peek--select-prev-file))

(use-package consult-lsp
  :defer t)

(use-package dap-mode
  :commands dap-debug
  :hook (dap-mode . dap-tooltip-mode)
  :config
  (require 'dap-node)
  (require 'dap-chrome)
  (require 'dap-firefox)
  (require 'dap-edge)
  (require 'dap-netcore)
  (require 'dap-lldb)
  (require 'dap-cpptools))

(use-package dap-ui
  :straight nil
  :after dap-mode
  :hook (dap-mode . dap-ui-mode)
  :hook (dap-ui-mode . dap-ui-controls-mode))

(use-package cheat-sh)

;; (use-package lsp-mssql
;;   :hook (sql-mode . #'lsp))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package tree-sitter
  :defer t
  :config
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)
  :init
  (global-tree-sitter-mode))

(use-package tree-sitter-langs
  :defer t)

(use-package tree-sitter-indent
  :defer t)

(use-package csharp-mode
  :mode (("\\.cs\\'" . csharp-tree-sitter-mode)
         ("\\.csx\\'" . csharp-tree-sitter-mode))
  :config
  (require 'dap-netcore))

(use-package sharper
  :bind ("C-c n" . sharper-main-transient))

(use-package sln-mode :mode "\\.sln\\'")

(use-package csproj-mode
  :straight (csproj-mode :type git :host github :repo "omajid/csproj-mode")
  :mode "\\.csproj\\'")

(use-package odin-mode
  :straight (odin-mode :type git :host github :repo "mattt-b/odin-mode")
  :mode "\\.odin\\'")

(use-package typescript-mode
  :mode "\\.ts\\'"
  :mode "\\.tsx\\'"
  :config
  (require 'dap-node)
  (require 'dap-chrome)
  (setq typescript-indent-level 2))

(use-package web-mode
  :mode "\\.html?\\'"
  :mode "\\.razor?\\'"
  :mode "\\.cshtml?\\'"
  :config
  (setq web-mode-engines-alist
	    '(("razor"    . "\\.cshtml\\'")
	      ("blade"  . "\\.blade\\.")
	      ("svelte" . "\\.svelte\\.")
          ))
  )

(use-package sass-mode
  :mode "\\.sass\\'")

(use-package css-mode
  :mode "\\.css\\'")

(use-package scss-mode
  :mode "\\.scss\\'")

(use-package go-mode
  :mode "\\.go\\'")

(use-package json-mode
  :mode "\\.js\\(?:on\\|[hl]int\\(?:rc\\)?\\)\\'")

(use-package yaml-mode
  :mode "Procfile\\'")

(use-package cc-mode)

(use-package rust-mode)

(use-package plantuml-mode
  :config
  (setq plantuml-jar-path "c:/tools/plantuml/plantuml.jar")
  (setq plantuml-default-exec-mode 'jar))

(use-package dockerfile-mode)

(use-package docker
  :defer t)

(use-package ahk-mode
  :hook (ahk-mode . rainbow-delimiters-mode))

;; (use-package
;;   :straight (lsp-tailwindcss :type git :host github :repo "merrickluo/lsp-tailwindcss"))

(use-package format-all)

(use-package magit
  :defer t
  :commands (magit-status magit-get-current-branch))

(defun +plantuml-org-babel-execute:plantuml-a (body params)
  "Execute a block of plantuml code with org-babel.
This function is called by `org-babel-execute-src-block'."
  (require 'plantuml-mode)
  ;; REVIEW Refactor me
  (let* ((body (replace-regexp-in-string
                "^[[:blank:]\n]*\\(@start\\)"
                "\\\\\\1"
                body))
         (fullbody (org-babel-plantuml-make-body body params))
         (out-file (or (cdr (assq :file params))
                       (org-babel-temp-file "plantuml-" ".png")))
         (in-file (org-babel-temp-file "plantuml-")))
    (if (eq plantuml-default-exec-mode 'server)
        (if (bound-and-true-p org-export-current-backend)
            (user-error "Exporting plantuml diagrams in server mode is not supported (see `plantuml-default-exec-mode')")
          (save-current-buffer
            (save-match-data
              (with-current-buffer
                  (url-retrieve-synchronously (plantuml-server-encode-url body)
                                              nil t)
                (goto-char (point-min))
                ;; skip the HTTP headers
                (while (not (looking-at "\n")) (forward-line))
                (delete-region (point-min) (+ 1 (point)))
                (write-file out-file)))))
      (let* ((cmd (concat (cond ((eq plantuml-default-exec-mode 'executable)
                                 (unless (executable-find plantuml-executable-path)
                                   (error "Could not find plantuml at %s"
                                          (executable-find plantuml-executable-path)))
                                 (concat (shell-quote-argument (executable-find plantuml-executable-path))
                                         " --headless"))
                                ((not (file-exists-p plantuml-jar-path))
                                 (error "Could not find plantuml.jar at %s" org-plantuml-jar-path))
                                ((concat "java " (cdr (assoc :java params)) " -jar "
                                         (shell-quote-argument
                                          (expand-file-name plantuml-jar-path)))))
                          " "
                          (pcase (file-name-extension out-file)
                            ("png" "-tpng")
                            ("svg" "-tsvg")
                            ("eps" "-teps")
                            ("pdf" "-tpdf")
                            ("tex" "-tlatex")
                            ("vdx" "-tvdx")
                            ("xmi" "-txmi")
                            ("scxml" "-tscxml")
                            ("html" "-thtml")
                            ("txt" "-ttxt")
                            ("utxt" "-utxt"))
                          " "
                          " -p " (cdr (assoc :cmdline params)) " < "
                          (org-babel-process-file-name in-file)
                          " > "
                          (org-babel-process-file-name out-file))))
        (with-temp-file in-file (insert fullbody))
        (message "%s" cmd)
        (org-babel-eval cmd "")))
    (unless (cdr (assq :file params))
      out-file)))
;; (require 'ob-plantuml)
(with-eval-after-load 'org
  (advice-add #'org-babel-execute:plantuml
              :override #'+plantuml-org-babel-execute:plantuml-a)

  (org-babel-do-load-languages 'org-babel-load-languages '((ruby . t)
                                                           (plantuml . t)
                                                           (emacs-lisp . t))))
(use-package org-mode
  :hook ((org-mode . org-fancy-priorities-mode))
  :config
  ;; (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
  ;; (org-babel-do-load-languages
  ;; 'org-babel-load-languages
  ;; '((plantuml . t)))
  ;; (org-babel-do-load-languages 'org-babel-load-languages '((plantuml . t)
  ;;                                                          (emacs-lisp . t)))
  (setq org-plantuml-jar-path "c:/tools/plantuml/plantuml.jar")
  (setq org-return-follows-link t)
  (setq org-superstar-special-todo-items t)
  (setq org-ellipsis " ▼")
  (setq org-todo-keywords
        '((sequence
           "TODO(t)"  ; A task that needs doing & is ready to do
           "PROJ(p)"  ; A project, which usually contains other tasks
           "LOOP(r)"  ; A recurring task
           "STRT(s)"  ; A task that is in progress
           "WAIT(w)"  ; Something external is holding up this task
           "HOLD(h)"  ; This task is paused/on hold because of me
           "IDEA(i)"  ; An unconfirmed and unapproved task or notion
           "|"
           "DONE(d)"  ; Task successfully completed
           "KILL(k)") ; Task was cancelled, aborted or is no longer applicable
          (sequence
           "[ ](T)"   ; A task that needs doing
           "[-](S)"   ; Task is in progress
           "[?](W)"   ; Task is being held up or paused
           "|"
           "[X](D)")  ; Task was completed
          (sequence
           "|"
           "OKAY(o)"
           "YES(y)"
           "NO(n)"))
        org-todo-keyword-faces
        '(("[-]"  . +org-todo-active)
          ("STRT" . +org-todo-active)
          ("[?]"  . +org-todo-onhold)
          ("WAIT" . +org-todo-onhold)
          ("HOLD" . +org-todo-onhold)
          ("PROJ" . +org-todo-project)
          ("NO"   . +org-todo-cancel)
          ("KILL" . +org-todo-cancel))))

;;;###autoload
(defun +org/table-previous-row ()
  "Go to the previous row (same column) in the current table. Before doing so,
re-align the table if necessary. (Necessary because org-mode has a
`org-table-next-row', but not `org-table-previous-row')"
  (interactive)
  (org-table-maybe-eval-formula)
  (org-table-maybe-recalculate-line)
  (if (and org-table-automatic-realign
           org-table-may-need-update)
      (org-table-align))
  (let ((col (org-table-current-column)))
    (beginning-of-line 0)
    (when (or (not (org-at-table-p)) (org-at-table-hline-p))
      (beginning-of-line))
    (org-table-goto-column col)
    (skip-chars-backward "^|\n\r")
    (when (org-looking-at-p " ")
      (forward-char))))


;;
;;; Row/Column insertion

;;;###autoload
(defun +org/table-insert-column-left ()
  "Insert a new column left of the current column."
  (interactive)
  (org-table-insert-column)
  (org-table-move-column-left))

;;;###autoload
(defun +org/table-insert-row-below ()
  "Insert a new row below the current row."
  (interactive)
  (org-table-insert-row 'below))


;;
;;; Hooks

;;;###autoload
(defun +org-realign-table-maybe-h ()
  "Auto-align table under cursor."
  (when (and (org-at-table-p) org-table-may-need-update)
    (let ((pt (point))
          (inhibit-message t))
      (if org-table-may-need-update (org-table-align))
      (goto-char pt))))

;;;###autoload
(defun +org-enable-auto-reformat-tables-h ()
  (message "Hooks")
  (add-hook 'meow-insert-state-exit-hook #'+org-realign-table-maybe-h nil t))

;;;###autoload
(defun +org-delete-backward-char-and-realign-table-maybe-h ()
  "Ensure deleting characters with backspace doesn't deform the table cell."
  (when (eq major-mode 'org-mode)
    (org-check-before-invisible-edit 'delete-backward)
    (save-match-data
      (when (and (org-at-table-p)
                 (not (org-region-active-p))
                 (string-match-p "|" (buffer-substring (point-at-bol) (point)))
                 (looking-at-p ".*?|"))
        (let ((pos (point))
              (noalign (looking-at-p "[^|\n\r]*  |"))
              (c org-table-may-need-update))
          (delete-char -1)
          (unless overwrite-mode
            (skip-chars-forward "^|")
            (insert " ")
            (goto-char (1- pos)))
          ;; noalign: if there were two spaces at the end, this field
          ;; does not determine the width of the column.
          (when noalign (setq org-table-may-need-update c)))
        t))))


;;
;;; Advice

;;;###autoload
(defun +org-realign-table-maybe-a (&rest _)
  "Auto-align table under cursor and re-calculate formulas."
  (when (eq major-mode 'org-mode)
    (+org-realign-table-maybe-h)))

(use-package org-superstar
  :hook ((org-mode . org-superstar-mode))
  :init
  (setq org-superstar-todo-bullet-alist
        '(("TODO" "☐　")
          ("NEXT" "✒　")
          ("PROG" "✰　")
          ("WAIT" "☕　")
          ("FAIL" "✘　")
          ("DONE" "✔　")))
  (setq org-superstar-leading-bullet ?\s
        org-superstar-leading-fallback ?\s
        ;; org-superstar-remove-leading-stars t
        org-hide-leading-stars nil))

(use-package toc-org
  :hook (org-mode . toc-org-mode))

(use-package org-fancy-priorities
  :hook ((org-mode org-agenda-mode) . org-fancy-priorities-mode))

(use-package emojify
  :hook (after-init . global-emojify-mode))

(use-package hl-todo
  :hook ((prog-mode . hl-todo-mode)
         (fundamental-mode . hl-todo-mode)
         (org-mode . hl-todo-mode))
  :config
  (setq hl-todo-highlight-punctuation ":"
        hl-todo-keyword-faces
        `(;; For things that need to be done, just not today.
          ("TODO" warning bold)
          ;; For problems that will become bigger problems later if not
          ;; fixed ASAP.
          ("FIXME" error bold)
          ;; For tidbits that are unconventional and not intended uses of the
          ;; constituent parts, and may break in a future update.
          ("HACK" font-lock-constant-face bold)
          ;; For things that were done hastily and/or hasn't been thoroughly
          ;; tested. It may not even be necessary!
          ("REVIEW" font-lock-keyword-face bold)
          ;; For especially important gotchas with a given implementation,
          ;; directed at another user other than the author.
          ("NOTE" success bold)
          ;; For things that just gotta go and will soon be gone.
          ("DEPRECATED" font-lock-doc-face bold)
          ;; For a known bug that needs a workaround
          ("BUG" error bold)
          ;; For warning about a problematic or misguiding code
          ("XXX" font-lock-constant-face bold))))

(use-package pdf-tools
  :hook (pdf-view-mode . (lambda () (beacon-mode -1)))
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :custom
  ((pdf-info-epdfinfo-program "c:/tools/epdfinfo/epdfinfo.exe")))

(use-package saveplace-pdf-view)

(defun no-xls (&optional filename)
  (interactive "File Name: ")
  (when (and filename
             (not (buffer-file-name)))
    (write-file (make-temp-file filename)))
  (erase-buffer)
  (shell-command
   (format "c:\\tools\\xlhtml\\xlhtml.exe -nc -te %s | w3m -dump -T text/html" (buffer-file-name))
   (current-buffer))
  (setq buffer-file-name nil)
  (set-buffer-modified-p nil))

(add-to-list 'auto-mode-alist '("\\.xls\\'" . no-xls))
(add-to-list 'auto-mode-alist '("\\.xlsx\\'" . no-xls))

(use-package yasnippet
  :defer t
  :config
  (defun check-expansion ()
    (save-excursion
      (if (looking-at "\\_>") t
        (backward-char 1)
        (if (looking-at "\\.") t
          (backward-char 1)
          (if (looking-at "->") t nil)))))

  (defun do-yas-expand ()
    (let ((yas/fallback-behavior 'return-nil))
      (yas/expand)))

  (defun tab-indent-or-complete ()
    (interactive)
    (if (minibufferp)
        (minibuffer-complete)
      (if (or (not yas/minor-mode)
              (null (do-yas-expand)))
          (if (check-expansion)
              (company-complete-common)
            (indent-for-tab-command)))))

  (global-set-key (kbd "C-<return>") 'tab-indent-or-complete)
  :init
  (defvar user-snippets (concat user-emacs-directory "snippets/"))
  (setq yas-snippet-dirs '(user-snippets))
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :after yasnippet)

(use-package auto-yasnippet
  :defer t)

(use-package yatemplate
  :defer t
  :config
  (yatemplate-fill-alist)
  (auto-insert-mode +1))

(setq gc-cons-thershold (* 2 1000 1000))
