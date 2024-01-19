(use-package projectile
  :defer t
  :bind (:map projectile-mode-map
              ("C-x p" . projectile-command-map ))
  :init
  (setq projectile-enable-caching (not noninteractive)
        projectile-indexing-method 'hybrid
        projectile-auto-discover nil
        projectile-project-search-path project-dirs)

  (add-hook 'kill-emacs-hook 'projectile-discover-projects-in-search-path)
  (global-set-key [remap evil-jump-to-tag] #'projectile-find-tag)
  (global-set-key [remap find-tag]         #'projectile-find-tag)
  (letf! ((#'projectile--cleanup-known-projects #'ignore))
    (projectile-mode +1))
  )

(use-package perspective
  :custom
  (persp-mode-prefix-key (kbd "C-c C-p"))
  :config
  (setq persp-modestring-dividers '(" "))
  (add-hook 'persp-created-hook (lambda () (split-window-horizontally)))
  (setq persp-nil-name "main"
        persp-modestring-short t
        persp-set-last-persp-for-new-frames t)

  (persp-mode))

(use-package consult-projectile
  :straight (consult-projectile :type git :host gitlab :repo "OlMon/consult-projectile"))

(use-package persp-projectile)
