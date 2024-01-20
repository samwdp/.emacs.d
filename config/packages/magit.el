;; -*- lexical-binding: t; -*-
(use-package magit
  :defer t
  :hook (magit-mode . (lambda ()
                        (evil-collection-define-key 'normal 'magit-mode-map (kbd "C-k") nil)
                        (evil-collection-define-key 'normal 'magit-mode-map (kbd "C-j") nil)
                        ))
  :config
  (evil-collection-magit-setup)
  :commands (magit-status magit-get-current-branch))
