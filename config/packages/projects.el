(defvar project-dirs)
(when IS-WINDOWS
  (setq project-dirs '(("D:/work" . 3)
                       ("D:/projects" . 3)))
  )
(when IS-LINUX
  (setq project-dirs '(("~/work/" . 3)
                       ("~/projects/" . 3)))
  )
(use-package projectile
  :defer t
  :config
  (setq projectile-indexing-method 'hybrid)
  (add-hook 'kill-emacs-hook 'projectile-discover-projects-in-search-path)
  (define-key projectile-mode-map (kbd "C-x p") 'projectile-command-map)
  (projectile-mode +1)
  :init
  (setq projectile-enable-caching (not noninteractive)
        projectile-auto-discover nil)
    (setq projectile-project-search-path project-dirs))

(use-package perspective
  :custom
  (persp-mode-prefix-key (kbd "C-c C-p"))
  :config
  (add-hook 'persp-created-hook (lambda () (split-window-horizontally)))
  (setq persp-nil-name "main"
        persp-modestring-short t)
  (persp-mode))

(use-package consult-projectile
  :init (slot/vc-install :fetcher "gitlab" :repo "OlMon/consult-projectile"))

(use-package persp-projectile)
