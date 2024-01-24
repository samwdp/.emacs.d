;; -*- lexical-binding: t; -*-
(when IS-LINUX
  (use-package vterm
    :bind (:map vterm-mode-map
                ("C-l" . windmove-right)
                ("C-j" . windmove-down)
                ("C-k" . windmove-up)
                ("C-h" . windmove-left))))

(use-package eshell
  :straight nil
  :hook (eshell-mode . (lambda ()
                         (evil-collection-define-key 'normal 'eshell-mode-map (kbd "C-k") nil)
                         (evil-collection-define-key 'normal 'eshell-mode-map (kbd "C-j") nil)))
  :init

  (setq eshell-aliases-file (concat user-emacs-directory "eshell/aliases")
        eshell-rc-script (concat user-emacs-directory "eshell/profile")
        eshell-login-script (concat user-emacs-directory "eshell/login"))
  :config
  (evil-collection-define-key 'normal 'eshell-mode-map (kbd "C-k") nil)
  (evil-collection-define-key 'normal 'eshell-mode-map (kbd "C-j") nil)
  (unbind-key (kbd "C-j") eshell-mode-map)
  (unbind-key (kbd "C-k") eshell-mode-map)
  (unbind-key (kbd "C-h") eshell-mode-map)
  (unbind-key (kbd "C-l") eshell-mode-map)
  (unbind-key (kbd "C-j") eshell-prompt-mode-map)
  (unbind-key (kbd "C-k") eshell-prompt-mode-map)
  (unbind-key (kbd "C-h") eshell-prompt-mode-map)
  (unbind-key (kbd "C-l") eshell-prompt-mode-map))

(use-package eshell-up)

(use-package eshell-syntax-highlighting
  :hook (eshell-mode . eshell-syntax-highlighting-mode))

(use-package eshell-prompt-extras
  :config
  (setq eshell-highlight-prompt nil
        eshell-promtp-function 'epe-theme-multiline-with-status))

(use-package eat
  :bind (:map eat-mode-map
              ("C-l" . windmove-right)
              ("C-j" . windmove-down)
              ("C-k" . windmove-up)
              ("C-h" . windmove-left)))

(use-package shell-switcher)
