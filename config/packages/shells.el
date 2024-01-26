;; -*- lexical-binding: t; -*-
(when IS-LINUX
  (use-package vterm
    :bind (:map vterm-mode-map
                ("C-l" . windmove-right)
                ("C-j" . windmove-down)
                ("C-k" . windmove-up)
                ("C-h" . windmove-left))))

(defun sp/configure-eshell ()
  (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)
  (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)

  (evil-collection-define-key 'normal 'eshell-mode-map (kbd "C-k") nil)
  (evil-collection-define-key 'normal 'eshell-mode-map (kbd "C-j") nil)
  (evil-collection-define-key 'insert 'eshell-mode-map (kbd "-") nil)

  (unbind-key (kbd "C-j") eshell-mode-map)
  (unbind-key (kbd "C-k") eshell-mode-map)
  (unbind-key (kbd "C-h") eshell-mode-map)
  (unbind-key (kbd "C-l") eshell-mode-map)
  (unbind-key (kbd "C-j") eshell-prompt-mode-map)
  (unbind-key (kbd "C-k") eshell-prompt-mode-map)
  (unbind-key (kbd "C-h") eshell-prompt-mode-map)
  (unbind-key (kbd "C-l") eshell-prompt-mode-map)

  (setq eshell-history-size 10000
        eshell-buffer-maximum-lines 10000
        eshell-hist-ignoredups t
        eshell-scroll-to-bottom-on-input t)
  )

(use-package eshell-prompt-extras)
(use-package eshell
  :straight nil
  :hook (eshell-first-time-mode . sp/configure-eshell)
  :init
  (setq eshell-aliases-file (concat user-emacs-directory "eshell/aliases"))
  ;; eshell-rc-script (concat user-emacs-directory "eshell/profile")
  ;; eshell-login-script (concat user-emacs-directory "eshell/login"))
  :config
  (setq eshell-prompt-function 'epe-theme-multiline-with-status)
  )

(use-package eshell-up)

(use-package eshell-syntax-highlighting
  :hook (eshell-mode . eshell-syntax-highlighting-mode))
