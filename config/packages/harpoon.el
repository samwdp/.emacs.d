(use-package harpoon
  :config
  (global-set-key (kbd "M-RET") 'harpoon-add-file)
  (global-set-key (kbd "M-<return>") 'harpoon-add-file)
  (global-set-key (kbd "C-c h RET") 'harpoon-add-file)
  (global-set-key (kbd "C-c h f") 'harpoon-toggle-file)
  (global-set-key (kbd "C-c h h") 'harpoon-toggle-quick-menu)
  (global-set-key (kbd "C-c h c") 'harpoon-clear)
  (global-set-key (kbd "M-n") 'harpoon-go-to-1)
  (global-set-key (kbd "M-o") 'harpoon-go-to-3)
  (global-set-key (kbd "M-e") 'harpoon-go-to-2)
  (global-set-key (kbd "M-i") 'harpoon-go-to-4)
  (setq harpoon-project-package  'projectile
        harpoon-separate-by-branch t))
