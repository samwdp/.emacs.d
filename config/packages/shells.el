;; -*- lexical-binding: t; -*-
(when IS-LINUX
  (use-package vterm
    :bind (:map vterm-mode-map
                ("C-l" . windmove-right)
                ("C-j" . windmove-down)
                ("C-k" . windmove-up)
                ("C-h" . windmove-left)
                )))

(use-package eshell
  :straight nil
  :hook (eshell-mode . (lambda ()
                         (evil-collection-define-key 'normal 'eshell-mode-map (kbd "C-k") nil)
                         (evil-collection-define-key 'normal 'eshell-mode-map (kbd "C-j") nil)
                         ))
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
  (unbind-key (kbd "C-l") eshell-prompt-mode-map)
  )

(use-package eat
  :bind (:map eat-mode-map
              ("C-l" . windmove-right)
              ("C-j" . windmove-down)
              ("C-k" . windmove-up)
              ("C-h" . windmove-left)))

(use-package shell-switcher)
