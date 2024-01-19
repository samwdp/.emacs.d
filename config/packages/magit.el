(use-package magit
  :defer t
  :config
  (evil-collection-magit-setup)
  :commands (magit-status magit-get-current-branch))
