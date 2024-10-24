;; -*- lexical-binding: t; -*-
(use-package evil
  :config
  (defun sp/evil-yank-advice (orig-fn beg end &rest args)
    (require 'pulsar)
    (pulsar--pulse nil nil beg end)
    (apply orig-fn beg end args))

  (advice-add 'evil-yank :around 'sp/evil-yank-advice)
  (evil-global-set-key 'normal (kbd "g d") 'lookup-definition)
  (evil-global-set-key 'normal (kbd "g i") 'lookup-implementation)
  (evil-global-set-key 'normal (kbd "g r") 'lookup-reference)
  (evil-global-set-key 'normal (kbd "g t") 'lookup-type-definition)
  (evil-global-set-key 'normal (kbd "g c c") 'comment-line)
  (evil-global-set-key 'visual (kbd "g c") 'comment-or-uncomment-region)
  (evil-global-set-key 'insert (kbd "C-p") nil)
  (evil-global-set-key 'insert (kbd "C-j") nil)
  (evil-global-set-key 'insert (kbd "C-k") nil)
  (evil-global-set-key 'insert (kbd "C-h") nil)
  (evil-global-set-key 'insert (kbd "C-l") nil)
  (evil-global-set-key 'normal (kbd "C-p") nil)
  (evil-global-set-key 'normal (kbd "C-j") nil)
  (evil-global-set-key 'normal (kbd "C-k") nil)
  (evil-global-set-key 'normal (kbd "C-h") nil)
  (evil-global-set-key 'normal (kbd "C-l") nil)
  (evil-global-set-key 'normal "-" 'dired-jump)
  :init      ;; tweak evil's configuration before loading it
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil)
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below t)
  (evil-mode))

(use-package evil-collection
  :after evil
  :custom (evil-collection-setup-minibuffer t)
  :config
  (evil-collection-init))

(use-package evil-surround
  :init
  (global-evil-surround-mode))
