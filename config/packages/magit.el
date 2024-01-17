(use-package magit
  :defer t
  :bind (:map magit-section-mode-map
              ("C-j" . windmove-down)
              ("C-k" . windmove-up)
              )
  :config
  (evil-collection-magit-setup)
  :commands (magit-status magit-get-current-branch))
