(setq gc-cons-thershold 100000000)
(setq read-process-output-max (* 1024 1024)) ;; 1mb
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))
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
      idle-update-delay 0.02
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
      mode-line-end-spaces nil
      set-language-environment "UTF-8")

(package-initialize)

(defconst NATIVECOMP (if (fboundp 'native-comp-available-p) (native-comp-available-p)))
(defconst IS-MAC     (eq system-type 'darwin))
(defconst IS-LINUX   (eq system-type 'gnu/linux))
(defconst IS-WINDOWS (memq system-type '(cygwin windows-nt ms-dos)))
(defconst EVIL-ON t)
(when IS-WINDOWS (setq package-gnupghome-dir (concat user-emacs-directory "elpa/gnupg/pubring.kbx")))

(unless package-archive-contents
  (package-refresh-contents))

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))
(when (daemonp)
  (exec-path-from-shell-initialize))
(cl-defun slot/vc-install (&key (fetcher "github") repo name rev backend)
  "Install a package from a remote if it's not already installed.
This is a thin wrapper around `package-vc-install' in order to
make non-interactive usage more ergonomic.  Takes the following
named arguments:

- FETCHER the remote where to get the package (e.g., \"gitlab\").
  If omitted, this defaults to \"github\".

- REPO should be the name of the repository (e.g.,
  \"slotThe/arXiv-citation\".

- NAME, REV, and BACKEND are as in `package-vc-install' (which
  see)."
  (let* ((url (format "https://www.%s.com/%s" fetcher repo))
         (iname (when name (intern name)))
         (pac-name (or iname (intern (file-name-base repo)))))
    (unless (package-installed-p pac-name)
      (package-vc-install url iname rev backend))))


(defun sp/defer-garbage-collection-h ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun sp/restore-garbage-collection-h ()
  ;; Defer it so that commands launched immediately after will enjoy the
  ;; benefits.
  (run-at-time
   1 nil (lambda () (setq gc-cons-threshold 16777216))))

(add-hook 'minibuffer-setup-hook #'sp/defer-garbage-collection-h)
(add-hook 'minibuffer-exit-hook #'sp/restore-garbage-collection-h)

(setq emacs-version-short (replace-regexp-in-string
                           "\\([0-9]+\\)\\.\\([0-9]+\\).*"
                           "\\1_\\2" emacs-version))
(setq custom-file (expand-file-name
                   (concat "custom_" emacs-version-short ".el")
                   user-emacs-directory))

(add-to-list 'load-path (expand-file-name "custom/" user-emacs-directory))

;; (server-start)
(defun efs/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                    (time-subtract after-init-time before-init-time)))
           gcs-done))

(when NATIVECOMP
  (setq native-comp-async-report-warnings-errors nil)
  (add-to-list 'native-comp-eln-load-path (expand-file-name "eln-cache/" user-emacs-directory)))

(setq use-package-always-ensure t)

(use-package gcmh
  :ensure t
  :config
  (gcmh-mode 1))
(use-package unicode-fonts
  :init (slot/vc-install :fetcher "github" :repo "rolandwalker/unicode-fonts"))

(use-package fancy-battery
  :ensure t
  :hook (after-init . fancy-battery-mode))

(defun toggle-transparency () )

(defun transparency (value)
  "Sets the transparency of the frame window. 0=transparent/1.0=opaque"
  (interactive "nTransparency Value 0 - 1 opaque: t")
  (set-frame-parameter (selected-frame) 'alpha-background value))

(defvar sp/text-height 18)
;; (defvar sp/text-height 28)
(defvar sp/text-height-variable 20)
(defvar sp/font-string "FiraCode Nerd Font")

(defun sp/new-frame ()
  (set-face-attribute 'default nil :font (font-spec :family sp/font-string :size sp/text-height))
  (set-face-attribute 'fixed-pitch nil :font (font-spec :family sp/font-string :size sp/text-height))
  (set-face-attribute 'fixed-pitch-serif nil :font (font-spec :family sp/font-string :size sp/text-height))
  (set-face-attribute 'variable-pitch nil :font (font-spec :family sp/font-string :size sp/text-height-variable))
  (set-frame-parameter (selected-frame) 'alpha-background 0.9 ))

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
              tab-width 4
              fill-column 80)
(global-display-fill-column-indicator-mode)
(setq-default tab-always-indent nil)
(setq-default display-line-numbers-type 'relative)

(add-hook 'prog-mode-hook 'display-line-numbers-mode)

(global-set-key (kbd "C-h") 'windmove-left)
(global-set-key (kbd "C-j") 'windmove-down)
(global-set-key (kbd "C-k") 'windmove-up)
(global-set-key (kbd "C-l") 'windmove-right)
(global-set-key (kbd "C-SPC") 'completion-at-point)
;; (global-set-key (kbd "C-.") lsp-execute-code-action)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "M-RET") 'harpoon-add-file)

(defun spawn-shell (name)
  (interactive "MName of new shell: ")
  (pop-to-buffer (get-buffer-create (generate-new-buffer-name name)))
  (term (current-buffer)))

(defun eshell-with-name ()
  (interactive)
  (let* ((eshell-buffer-names (mapcar (lambda (buf)
					                    (buffer-name buf))
					                  (buffer-list)))
	     (match (completing-read "eshell buffers: "
				                 eshell-buffer-names
                                 (lambda (buf)
				                   (string-match-p "*eshell*" buf))))
	     (eshell-buffer-exists (member match eshell-buffer-names)))
    (if eshell-buffer-exists
	    (switch-to-buffer match)
	  (eshell 99)
	  (rename-buffer (concat "*eshell*<" match ">")))))

(use-package no-littering
  :ensure t
  :config
  (setq auto-save-file-name-transforms
	    `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))

(setq custom-theme-directory (concat user-emacs-directory "themes/"))

(use-package doom-themes
  :ensure t
  :init (load-theme 'gruvbox-sp t))

(use-package doom-modeline
  :ensure t
  :hook (doom-modeline-mode . size-indication-mode)
  :hook (doom-modeline-mode . column-number-mode)
  :config
  (setq doom-modeline-bar-width 3
        doom-modeline-github nil
        doom-modeline-mu4e nil
        doom-modeline-persp-name t
        doom-modeline-buffer-encoding nil
        doom-modeline-workspace-name nil
        doom-modeline-minor-modes nil
        doom-modeline-major-mode-icon nil
        doom-modeline-vcs-max-length 50
        doom-modeline-buffer-file-name-style 'truncate-all
        ;; Only show file encoding if it's non-UTF-8 and different line endings
        ;; than the current OSes preference
        doom-modeline-default-eol-type
        (cond (IS-MAC 2)
              (IS-WINDOWS 1)
              (0)))
  :init
  (doom-modeline-mode))

(use-package which-key
  :ensure t
  :defer 0
  :diminish which-key-mode
  :init (which-key-mode)
  :config
  (setq which-key-idle-delay 0.0))

(defun lookup-definition ())
(defun lookup-reference ())
(defun lookup-implementation ())
(defun lookup-declaration ())
(defun lookup-type-definition ())
(defun lookup-doc ())
(defun sp/format-buffer ())
(global-set-key [remap lookup-definition] #'xref-find-definitions)
(global-set-key [remap lookup-reference] #'xref-find-references)
;; (global-set-key [remap sp/format-buffer] #'format-all-buffer)
(when EVIL-ON

  (defun sp/evil-yank-advice (orig-fn beg end &rest args)
    (pulse-momentary-highlight-region beg end)
    (apply orig-fn beg end args))

  (use-package evil
    :config
    (advice-add 'evil-yank :around 'sp/evil-yank-advice)
    (evil-global-set-key 'normal (kbd "g d") 'lookup-definition)
    (evil-global-set-key 'normal (kbd "g i") 'lookup-implementation)
    (evil-global-set-key 'normal (kbd "g r") 'lookup-reference)
    (evil-global-set-key 'normal (kbd "g t") 'lookup-type-definition)
    :init      ;; tweak evil's configuration before loading it
    (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
    (setq evil-want-keybinding nil)
    (setq evil-vsplit-window-right t)
    (setq evil-split-window-below t)
    (evil-mode))

  (use-package evil-collection
    :after evil
    :config
    (evil-collection-init))

  (use-package evil-tutor)

  (use-package general
    :config
    (general-evil-setup)

    ;; set up 'SPC' as the global leader key
    (general-create-definer sp/leader-keys
      :states '(normal insert visual emacs)
      :keymaps 'override
      :prefix "SPC" ;; set leader
      :global-prefix "M-SPC") ;; access leader in insert mode

    (sp/leader-keys
      "b" '(:ignore t :wk "buffer")
      "bb" '(consult-project-buffer :wk "Switch buffer")
      "bd" '(kill-this-buffer :wk "Switch buffer")
      "bB" '(consult-buffer :wk "all buffers")
      "bk" '(kill-this-buffer :wk "Kill this buffer")
      "bn" '(next-buffer :wk "Next buffer")
      "bp" '(previous-buffer :wk "Previous buffer")
      "br" '(revert-buffer :wk "Reload buffer"))

    (sp/leader-keys
      "." '(find-file :wk "find files")
      "SPC" '(consult-projectile-find-file :wk "find files")
      "TAB" '(persp-switch :wk "switch project")
      )

    (sp/leader-keys
      "e" '(:ignore t :wk "eval")
      "eb" '(eval-buffer :wk "eval buffer")
      )

    (sp/leader-keys
      "g" '(:ignore t :wk "git")
      "gs" '(magit-status :wk "magit status")
      )

    (sp/leader-keys
      "w" '(save-buffer :wk "save")
      "f" '(format-all-buffer :wk "format buffer")
      "ei" '((lambda ()
               (interactive)
               (find-file (expand-file-name (concat user-emacs-directory "init.el"))))
             :wk "emacs config")
      "sp" '( consult-ripgrep :wk "search")
      "ss" '( consult-line :wk "find line")
      )

    (sp/leader-keys
      "p" '(:ignore t :wk "project")
      "pc" '(projectile-compile-project
             t :wk "project")
      "pp" '(projectile-switch-project :wk "switch project")
      "pd" '(persp-kill :wk "project kill")
      "ps" '(projectile-discover-projects-in-search-path :wk "discover project")
      "pr" '(recompile :wk "recompile")
      "pi" '(projectile-invalidate-cache :wk "invalidate cache")
      "op" '(+treemacs/toggle :wk "project filetree")
      )
    )
  )
(unless EVIL-ON
  (use-package meow
    :ensure t
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
       '("bb" . consult-project-buffer)
       '("bd" . kill-this-buffer)
       '("bB" . consult-buffer)
       '("cc" . projectile-compile-project)
       '("db" . killthis-buffer)
       '("dp" . persp-kill)
       '("ed" . eval-defun)
       '("is" . consult-yasnippet)
       '("ic" . insert-char)
       '("og" . magit-status)
       '("pp" . projectile-switch-project)
       '("pd" . persp-kill)
       '("pc" . projectile-compile-project)
       '("ps" . projectile-discover-projects-in-search-path)
       '("pr" . recompile)
       '("pi" . projectile-invalidate-cache)
       '("op" . +treemacs/toggle)
       '("os" . eshell-with-name)
       (if IS-LINUX
           '("ot" . vterm)
         '("ot" . shell))
       '("w" . save-buffer)
       '("ff" . format-all-buffer)
       '("fde" . (lambda ()
                   (interactive)
                   (find-file (expand-file-name (concat user-emacs-directory "init.el")))))
       '("sp" . consult-ripgrep)
       '("ss" . consult-line)
       '("tt" . transparency)
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
       '("v d" . lookup-definition)
       '("v r" . lookup-reference)
       '("v i" . lookup-implementation)
       '("v e" . lookup-declaration)
       '("v t" . lookup-type-definition)
       '("v v" . lookup-doc)
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
  )
(when IS-LINUX
  (use-package vterm
    :ensure t
    :config
    (define-key vterm-mode-map (kbd "C-l") 'windmove-right)
    (define-key vterm-mode-map (kbd "C-j") 'windmove-down)
    (define-key vterm-mode-map (kbd "C-k") 'windmove-up)
    (define-key vterm-mode-map (kbd "C-h") 'windmove-left)
    ))

(use-package all-the-icons)

(use-package all-the-icons-completion
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init
  (all-the-icons-completion-mode))

(use-package all-the-icons-dired
  :defer t)

(use-package smartparens
  :defer t)

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

(use-package harpoon
  :config
  (global-set-key (kbd "C-c h RET") 'harpoon-add-file)
  (global-set-key (kbd "C-c h f") 'harpoon-toggle-file)
  (global-set-key (kbd "C-c h h") 'harpoon-toggle-quick-menu)
  (global-set-key (kbd "C-c h c") 'harpoon-clear)
  (global-set-key (kbd "M-n") 'harpoon-go-to-1)
  (global-set-key (kbd "M-o") 'harpoon-go-to-3)
  (global-set-key (kbd "M-e") 'harpoon-go-to-2)
  (global-set-key (kbd "M-i") 'harpoon-go-to-4)
  (setq harpoon-project-package  'projectile
        harpoon-separate-by-branch t)
  )

(use-package adaptive-wrap)

;; (use-package topsy
;;   :straight (topsy :type git :host github :repo "alphapapa/topsy.el" :branch "master")
;;   :hook (prog-mode . topsy-mode))


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

(use-package corfu

  :custom
  (tab-always-indent 'complete-tag)
  (corfu-separator ?\s)
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-prefix 1)
  (corfu-auto-delay 0.0)
  (corfu-popupinfo-delay 0.3)
  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode))

(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-tex))

(use-package kind-icon
  :after corfu
  :custom
  (kind-icon-use-icons t)
  (kind-icon-default-face 'corfu-default) ; Have background color be the same as `corfu' face background
  (kind-icon-blend-background nil)  ; Use midpoint color between foreground and background colors ("blended")?
  (kind-icon-blend-frac 0.08)

  ;; NOTE 2022-02-05: `kind-icon' depends `svg-lib' which creates a cache
  ;; directory that defaults to the `user-emacs-directory'. Here, I change that
  ;; directory to a location appropriate to `no-littering' conventions, a
  ;; package which moves directories of other packages to sane locations.
  (svg-lib-icons-dir (no-littering-expand-var-file-name "svg-lib/cache/")) ; Change cache dir
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package vertico
  :hook (vertico-mode . vertico-posframe-mode)
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

;; when in a gui frame
(use-package vertico-posframe
  :after vertico
  :config
  (setq vertico-posframe-parameters
        '((left-fringe . 5)
          (right-fringe . 5)
          (alpha-background . 0.9)))
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
  (setq consult-preview-key "M-.")
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   :preview-key '(:debounce 0.4 any))

  (setq consult-narrow-key "<"))

(use-package embark-consult
  :after (embark consult))

;; (use-package chatgpt
;;   :init
;;   (slot/vc-install :fetcher "github" :repo "joshcho/ChatGPT.el.git")
;;   (require 'python)
;;   (setq chatgpt-repo-path (expand-file-name "elpa/ChatGPT.el/" user-emacs-directory))
;;   :bind ("C-c q" . chatgpt-query))

(use-package consult-dir)

(use-package consult-flycheck)

(use-package consult-projectile
  :init (slot/vc-install :fetcher "gitlab" :repo "OlMon/consult-projectile"))


(use-package flycheck
  :commands flycheck-list-errors flycheck-buffer
  :config
  (setq flycheck-display-errors-function 'ignore)
  (setq flycheck-emacs-lisp-load-path 'inherit)
  (delq 'new-line flycheck-check-syntax-automatically)
  (setq flycheck-idle-change-delay 1.0)
  (setq flycheck-buffer-switch-check-intermediate-buffers t)
  (setq flycheck-display-errors-delay 0.25))

(use-package flycheck-tip)

(use-package projectile
  :defer t
  :config
  (setq projectile-indexing-method 'hybrid)
  (define-key projectile-mode-map (kbd "C-x p") 'projectile-command-map)
  (projectile-mode +1)
  :init
  (setq projectile-enable-caching t)
  (when IS-WINDOWS
    (setq projectile-project-search-path '(("D:/work" . 4)
                                           ("D:/projects" . 4))))

  (when IS-LINUX
    (setq projectile-project-search-path '(("~/work/" . 4)
                                           ("~/projects/" . 4)))))

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
  :bind (:map treemacs-mode-map
              ("C-k" . windmove-up)
              ("C-j" . windmove-down))
  :init
  (setq treemacs-follow-after-init t
        treemacs-is-never-other-window nil
        treemacs-width 50
        treemacs-position 'left
        treemacs-sorting 'alphabetic-case-insensitive-asc)
  :config
  (treemacs-follow-mode -1))

(use-package treemacs-magit
  :after treemacs magit
  :bind (:map magit-status-mode-map
              ("K" . magit-discard)))

(use-package treemacs-perspective ;;treemacs-perspective if you use perspective.el vs. persp-mode
  :after (treemacs perspective) ;;or perspective vs. persp-mode
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

;; use lsp

(use-package eglot
  :hook (before-save . eglot-format-buffer)
  :hook (eglot-mode . eglot-inlay-hints-mode)
  :bind (:map eglot-mode-map
              ("C-." . 'eglot-code-actions))
  :config
  (add-hook 'before-save-hook
            (lambda () (eglot-format)))

  (setq completion-category-overrides '((eglot (styles orderless))))

  (define-key eglot-mode-map [remap lookup-definition] #'xref-find-definitions)
  (define-key eglot-mode-map [remap lookup-reference] #'xref-find-references)
  (define-key eglot-mode-map [remap lookup-implementation] #'eglot-find-implementation)
  (define-key eglot-mode-map [remap lookup-declaration] #'eglot-find-declaration)
  (define-key eglot-mode-map [remap lookup-type-definition] #'eglot-find-typeDefinition)
  (define-key eglot-mode-map [remap sp/format-buffer] #'eglot-format-buffer)
  (setq eglot-connect-timeout 90)
  (add-to-list 'eglot-server-programs '(odin-mode "c:/tools/ols/ols.exe"))
  ;; (add-to-list 'eglot-server-programs '(html-mode "tailwindcss-language-server"))
  ;; (add-to-list 'eglot-server-programs '((web-mode :language-id "html") . ("tailwindcss-language-server")))
  (add-to-list 'eglot-server-programs '(razor-mode "rzls"))
  (add-to-list 'eglot-server-programs '(web-mode "rzls"))
  ;; (add-to-list 'eglot-server-programs '(razor-mode . (eglot-alternatives '(("vscode-html-language-server" "--stdio") ("html-languageserver" "--stdio")))))

  ;; (add-to-list 'eglot-server-programs '(razor-mode "tailwindcss-language-server"))
  (add-to-list 'eglot-server-programs '(html-ts-mode . (eglot-alternatives '(("vscode-html-language-server" "--stdio") ("html-languageserver" "--stdio")))))
  ;; (add-to-list 'eglot-server-programs '(html-ts-mode "tailwindcss-language-server"))

  )

(use-package lsp-treemacs
  :after (treemacs lsp))

(use-package lsp-tailwindcss
  :init (slot/vc-install :fetcher "github" :repo "merrickluo/lsp-tailwindcss"))

(defun corfu-lsp-setup ()
  (setq-local completion-styles '(orderless)
              completion-category-defaults nil))

;; (require 'init-tabnine-capf)
(use-package lsp-mode
  :bind (:map lsp-mode-map
              ("C-." . 'lsp-execute-code-action))
  :hook (lsp-mode . +lsp-optimization-mode)
  :hook (lsp-mode . lsp-signature-mode)
  :hook (lsp-completion-mode . my/lsp-mode-setup-completion)
  :commands (lsp lsp-deferred)
  :custom
  (lsp-completion-provider :none) ;; we use Corfu!
  :init
  (defun my/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless)))
  (setq lsp-completion-provider nil)
  (setq lsp-keymap-prefix "C-c l")
  (setq lsp-headerline-breadcrumb-enable nil
        lsp-idle-delay 0.0
        lsp-headerline-breadcrumb-icons-enable nil
        lsp-keep-workspace-alive nil
        lsp-modeline-diagnostics-enable nil
        lsp-modeline-code-actions-enable nil
        lsp-lens-enable nil
        lsp-enable-which-key-integration t
        lsp-enable-file-watchers nil
        lsp-enable-folding nil
        lsp-enable-text-document-color nil
        lsp-enable-on-type-formatting nil)
  :config
  (add-hook 'before-save-hook
            (lambda () (lsp-format-buffer)))

  (add-hook 'lsp-mode-hook #'corfu-lsp-setup)
  (define-key lsp-mode-map [remap xref-find-apropos] #'consult-lsp-symbols)
  (define-key lsp-mode-map [remap lookup-implementation] #'lsp-goto-implementation)
  (define-key lsp-mode-map [remap lookup-declaration] #'lsp-find-declaration)
  (define-key lsp-mode-map [remap lookup-reference] #'lsp-find-references)
  (define-key lsp-mode-map [remap lookup-definition] #'lsp-find-definition)
  (define-key lsp-mode-map [remap lookup-type-definition] #'lsp-goto-type-definition)
  (define-key lsp-mode-map [remap lookup-doc] #'lsp-ui-doc-glance)
  (define-key lsp-mode-map [remap sp/format-buffer] #'lsp-format-buffer)
  (define-key lsp-mode-map (kbd "<f7>") 'lsp-ui-doc-focus-frame)

  (add-to-list 'lsp-language-id-configuration '(odin-mode . "odin"))
  (add-to-list 'lsp-language-id-configuration '("\\.razor\\'" . "razor"))

  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection "c:/tools/ols/ols.exe")
                    :major-modes '(odin-mode)
                    :server-id 'ols
                    :multi-root t))
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection "rzls")
                    :activation-fn (lsp-activate-on "razor")
                    ;; :priority -1
                    :server-id 'rzls
                    ;; :add-on? t
                    :multi-root t))


  (lsp-enable-which-key-integration t))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (defun toggle-doc ()
    (interactive)
    (cursor)
    )
  (setq
   lsp-ui-doc-enable nil
   lsp-ui-doc-position 'at-point
   lsp-ui-doc-show-with-cursor t
   lsp-signature-auto-activate t
   lsp-signature-render-documentation t
   lsp-headerline-breadcrumb-segments '(symbols)
   lsp-ui-sideline-show-code-actions nil
   lsp-ui-sideline-ignore-duplicate t
   lsp-ui-sideline-show-hover t
   lsp-ui-sideline-show-diagnostics t
   lsp-ui-sideline-actions-icon lsp-ui-sideline-actions-icon-default)
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)

  (when EVIL-ON
    (evil-define-key 'normal lsp-mode-map (kbd "K") 'lsp-ui-doc-glance))
  (define-key lsp-ui-peek-mode-map (kbd "j") #'lsp-ui-peek--select-next)
  (define-key lsp-ui-peek-mode-map (kbd "k") #'lsp-ui-peek--select-prev)
  (define-key lsp-ui-peek-mode-map (kbd "M-j") #'lsp-ui-peek--select-next-file)
  (define-key lsp-ui-peek-mode-map (kbd "M-j") #'lsp-ui-peek--select-prev-file))

(use-package consult-lsp
  :defer t)

(use-package dap-mode
  :bind (:map dap-mode-map
              ("<f10>" . 'dap-next)
              ("<f11>" . 'dap-step-in)
              ("<f5>" . 'dap-continue))
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
  :ensure nil
  :after dap-mode
  :hook (dap-mode . dap-ui-mode)
  :hook (dap-ui-mode . dap-ui-controls-mode))

(use-package consult-eglot
  :ensure t
  :defer t)

(use-package cheat-sh)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package treesit
  :ensure nil
  :commands (treesit-install-language-grammar nf/treesit-install-all-languages)
  :init
  (setq treesit-font-lock-level 4)
  (setq treesit-language-source-alist
        '((bash . ("https://github.com/tree-sitter/tree-sitter-bash"))
          (c . ("https://github.com/tree-sitter/tree-sitter-c"))
          (c-sharp . ("https://github.com/tree-sitter/tree-sitter-c-sharp"))
          (cpp . ("https://github.com/tree-sitter/tree-sitter-cpp"))
          (css . ("https://github.com/tree-sitter/tree-sitter-css"))
          (dockerfile . ("https://github.com/camdencheek/tree-sitter-dockerfile"))
          (go . ("https://github.com/tree-sitter/tree-sitter-go"))
          (html . ("https://github.com/tree-sitter/tree-sitter-html"))
          (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript"))
          (json . ("https://github.com/tree-sitter/tree-sitter-json"))
          (lua . ("https://github.com/Azganoth/tree-sitter-lua"))
          (make . ("https://github.com/alemuller/tree-sitter-make"))
          (ocaml . ("https://github.com/tree-sitter/tree-sitter-ocaml" "master" "ocaml/src"))
          (python . ("https://github.com/tree-sitter/tree-sitter-python"))
          (php . ("https://github.com/tree-sitter/tree-sitter-php"))
          (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src"))
          (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src"))
          (ruby . ("https://github.com/tree-sitter/tree-sitter-ruby"))
          (rust . ("https://github.com/tree-sitter/tree-sitter-rust"))
          (sql . ("https://github.com/m-novikov/tree-sitter-sql"))
          (toml . ("https://github.com/tree-sitter/tree-sitter-toml"))
          (yaml . ("https://github.com/ikatyang/tree-sitter-yaml"))
          (odin . ("https://github.com/ap29600/tree-sitter-odin"))
          (zig . ("https://github.com/GrayJack/tree-sitter-zig"))))
  :config
  (defun nf/treesit-install-all-languages ()
    "Install all languages specified by `treesit-language-source-alist'."
    (interactive)
    (let ((languages (mapcar 'car treesit-language-source-alist)))
      (dolist (lang languages)
	    (treesit-install-language-grammar lang)
	    (message "`%s' parser was installed." lang)
	    (sit-for 0.75)))))


(use-package treesit-auto
  :config
  (global-treesit-auto-mode))

;; languages

(use-package csv-mode
  :mode "\\.csv\\'"
  :hook (csv-mode . csv-align-mode))

(use-package csharp-mode
  :hook (csharp-ts-mode . lsp-deferred)
  :mode (("\\.cs\\'" . csharp-ts-mode)))

(use-package html-mode
  :ensure nil
  :mode (("\\.html\\'" . html-ts-mode)))

(use-package sql
  :ensure nil
  :config
  (setq sql-ms-program "sqlcmd"))

(use-package razor-mode
  :init (slot/vc-install :fetcher "github" :repo "samwdp/razor-mode")
  :mode ("\\.razor\\'" . razor-mode)
  :mode ("\\.cshtml\\'" . yas--direct-razor-mode)
  :config
  (remove-hook 'before-save-hook
               (lambda () (eglot-format))))

(use-package sharper
  :bind ("C-c n" . sharper-main-transient))

(use-package powershell)

(use-package csproj-mode
  :init (slot/vc-install :fetcher "github" :repo "omajid/csproj-mode")
  :mode "\\.csproj\\'")

(use-package odin-mode
  :hook (odin-mode . lsp-deferred)
  :init (slot/vc-install :fetcher "github" :repo "mattt-b/odin-mode")
  :mode "\\.odin\\'")

(use-package typescript-mode
  :hook (typescript-ts-mode . lsp-deferred)
  :mode (("\\.ts\\'" . typescript-ts-mode)
         ("\\.tsx\\'". typescript-ts-mode))
  :custom
  (typescript-indent-level 4)
  (typescript-ts-mode-indent-offset 4))

(use-package web-mode
  :mode "\\.html?\\'")

(use-package sass-mode
  :hook (sass-mode . lsp-deferred)
  :mode "\\.sass\\'")

(use-package css-mode
  :hook (css-ts-mode . lsp-deferred)
  :mode (("\\.css\\'" . css-ts-mode)))

(use-package scss-mode
  :hook (scss-mode . lsp-deferred)
  :mode "\\.scss\\'")

(use-package go-mode
  :hook (csharp-ts-mode . lsp-deferred)
  :mode (("\\.go\\'" . go-ts-mode)))

(use-package json-mode
  :hook (json-ts-mode . lsp-deferred)
  :mode (("\\.json\\'" . json-ts-mode))
  )

(use-package yaml-mode
  :hook (yaml-ts-mode . lsp-deferred)
  :mode (("\\.yaml" . yaml-ts-mode))
  :mode "Procfile\\'")

(use-package toml-mode
  :hook (toml-ts-mode . lsp-deferred)
  :mode (("\\.toml" . toml-ts-mode)))

(use-package cc-mode
  :hook (c-ts-mode . lsp-deferred)
  :mode (("\\.c\\'" . c-ts-mode)
         ("\\.h\\'" . c-ts-mode)))

(use-package lua-mode)

(use-package rust-mode
  :hook (rust-ts-mode . lsp-deferred)
  :mode (("\\.rs\\'" . rust-ts-mode)))

(use-package plantuml-mode
  :config
  (setq plantuml-jar-path "c:/tools/plantuml/plantuml.jar")
  (setq plantuml-default-exec-mode 'jar))

(use-package dockerfile-mode
  :mode (("\\.docker\\'" . dockerfile-ts-mode)))

(use-package docker
  :defer t)

(use-package ahk-mode
  :hook (ahk-mode . rainbow-delimiters-mode))

(use-package restclient
  :mode ("\\.http\\'" . restclient-mode))

;; (use-package ob-restclient)

(use-package ob-csharp
  :init (slot/vc-install :fetcher "github" :repo "samwdp/ob-csharp")
  :config
  (org-babel-do-load-languages 'org-babel-load-languages '((csharp . t))))

(use-package glsl-mode)

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
;; (restclient . t))))

(use-package org
  :hook ((org-mode . org-fancy-priorities-mode))
  :config
  ;; (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
  ;; (org-babel-do-load-languages
  ;; 'org-babel-load-languages
  ;; '((plantuml . t)))
  ;; (org-babel-do-load-languages 'org-babel-load-languages '((plantuml . t)
  ;;                                                          (emacs-lisp . t)))
  (setq org-plantuml-jar-path "c:/tools/plantuml/plantuml.jar")
  (setq org-return-follows-link nil)
  (setq org-startup-with-inline-images t)
  (setq org-superstar-special-todo-items t)
  (setq org-display-inline-images t)
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

;; (use-package org-superstar
;;   :hook ((org-mode . org-superstar-mode))
;;   :init
;;   (setq org-superstar-todo-bullet-alist
;;         '(("TODO" "☐　")
;;           ("NEXT" "✒　")
;;           ("PROG" "✰　")
;;           ("WAIT" "☕　")
;;           ("FAIL" "✘　")
;;           ("DONE" "✔　")))
;;   (setq org-superstar-leading-bullet ?\s
;;         org-superstar-leading-fallback ?\s
;;         ;; org-superstar-remove-leading-stars t
;;         org-hide-leading-stars nil))

(use-package org-modern
  :hook((org-mode . org-modern-mode)
        (org-agenda-finilize . org-modern-agenda)))

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

(use-package org-brain
  :config
  (setq org-brain-path "~/brain"))

(use-package polymode
  :hook (org-brain-visualize-mode . org-brain-polymode))
(use-package pdf-tools
  :hook (pdf-view-mode . (lambda () (beacon-mode -1)))
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :config
  (when IS-WINDOWS
    (setq pdf-info-epdfinfo-program "c:/tools/epdfino/epdfinfo.exe")))
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
  :init
  (yas-global-mode 1))

(use-package auto-yasnippet
  :defer t)

(use-package consult-yasnippet)

(use-package adaptive-word-wrap-mode
  :init (slot/vc-install :fetcher "github" :repo "samwdp/adaptive-word-wrap-mode")
  :hook (after-init . global-adaptive-word-wrap-mode))

(use-package which-func
  :init
  (which-function-mode 1)
  :config
  (setq mode-line-format (delete (assoc 'which-func-mode
                                        mode-line-format) mode-line-format)
        which-func-header-line-format '(which-func-mode ("" which-func-format)))
  (defadvice which-func-ff-hook (after header-line activate)
    (when which-func-mode
      (setq mode-line-format (delete (assoc 'which-func-mode
                                            mode-line-format) mode-line-format)
            header-line-format which-func-header-line-format))))

(use-package sideline
  :init
  (setq sideline-backends-right '(
                                  (sideline-flycheck . up)
                                  )
        sideline-backends-left '((sideline-eldoc . up))
        sideline-display-backend-name t))

(use-package sideline-flycheck)
(use-package sideline-eldoc
  :init (slot/vc-install :fetcher "github" :repo "ginqi7/sideline-eldoc"))

(use-package git-gutter
  :hook (prog-mode . git-gutter-mode))

(use-package git-gutter-fringe
  :config
  (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom))

(use-package yasnippet-snippets)
(use-package competitive-programming-snippets)
(setq gc-cons-thershold (* 2 1000 1000))
