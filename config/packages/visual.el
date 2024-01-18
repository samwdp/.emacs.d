(use-package dtrt-indent)
(use-package transient)
(use-package smartparens)
(use-package unicode-fonts
  :init
  (if (display-graphic-p)
      (unicode-fonts-setup-h (selected-frame))
    (add-hook 'after-make-frame-functions 'unicode-fonts-setup-h)))
(use-package adaptive-wrap)

(use-package doom-themes
  :init (load-theme 'gruvbox-sp t))

(use-package doom-modeline
  :hook (doom-modeline-mode . size-indication-mode)
  :hook (doom-modeline-mode . column-number-mode)
  :config
  (setq doom-modeline-bar-width 3
        doom-modeline-time nil
        doom-modeline-time-icon nil
        doom-modeline-github nil
        doom-modeline-major-mode-icon t
        doom-modeline-mu4e nil
        doom-modeline-persp-name t
        doom-modeline-persp-icon t
        doom-modeline-buffer-encoding nil
        doom-modeline-percent-position nil
        doom-modeline-position-line-format nil
        doom-modeline-position-column-format nil
        doom-modeline-minor-modes nil
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
  :init (which-key-mode))

(use-package all-the-icons
  :straight (all-the-icons :fetcher github :repo "domtronn/all-the-icons.el")
  :if (display-graphic-p))

(use-package all-the-icons-completion
  :after (marginalia all-the-icons)
  :init
  (add-hook 'marginalia-mode-hook #'all-the-icons-completion-marginalia-setup)
  (all-the-icons-completion-mode))

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode)
  :defer t)

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
  (setq kind-icon-default-style
        '(:padding 0 :stroke 0 :margin 0 :radius 0 :height 0.6 :scale 1.0 :background
                   nil)) ;; hack to fix overflowing icons on corfu

  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package pulsar
  :init (pulsar-global-mode +1))

(use-package drag-stuff
  :defer t
  :config
  (global-set-key (kbd "M-j") (lambda (arg) (interactive "p") (drag-stuff-down arg)
                                (if (bound-and-true-p lsp-mode)
                                    (lsp-format-buffer)
                                  (format-all-buffer))))
  (global-set-key (kbd "M-k") (lambda (arg) (interactive "p") (drag-stuff-up arg)
                                (if (bound-and-true-p lsp-mode)
                                    (lsp-format-buffer)
                                  (format-all-buffer))))
  :init
  (drag-stuff-global-mode +1))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package git-gutter
  :hook (prog-mode . git-gutter-mode))

(use-package git-gutter-fringe
  :config
  (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom))

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

(use-package adaptive-word-wrap-mode
  :straight (adaptive-word-wrap-mode :type git :host github :repo "samwdp/adaptive-word-wrap-mode")
  :hook (after-init . global-adaptive-word-wrap-mode))

(use-package format-all)

(use-package popup-mode
  :demand t
  :straight (popup-mode :host github :repo "aaronjensen/emacs-popup-mode")
  :hook (after-init . +popup-mode)
  :config
  (setq popup-mode-all-rule t)
  (set-popup-rules! '(("^\\*Process List\\*$"
                       :side bottom :select t :slot -1 :vslot -1 :size +popup-shrink-to-fit)
                      ("^\\*Buffer List\\*$"
                       :side bottom :select t :slot -1 :vslot -1 :size +popup-shrink-to-fit)
                      ("^\\(?:\\*magit\\|magit:\\| \\*transient\\*\\)"
                       :side bottom :select nil :slot -1 :vslot -1 :height 0.4 :ttl nil)
                      ("^\\*Warnings\\*$"
                       :side bottom :select nil :slot -1 :vslot -1 :height 0.4 :ttl nil)
                      ("^\\*eshell\\*$"
                       :side bottom :select nil :slot -1 :vslot -1 :height 0.4 :ttl nil)
                      ("^\\*vterm\\*$"
                       :side bottom :select nil :slot -1 :vslot -1 :height 0.4 :ttl nil)
                      ("^\\*Help\\*$"
                       :side bottom :select nil :slot -1 :vslot -1 :height 0.4 :ttl nil)
                      ("Output\\*$"
                       :side bottom :select nil :slot -1 :vslot -1 :height 0.4 :ttl nil)
                      ("^\\*Messages\\*$"
                       :side bottom :select t :slot -1 :vslot -1 :height 0.3 :ttl nil))))


(use-package golden-ratio
  :init
  (golden-ratio-mode +1))
