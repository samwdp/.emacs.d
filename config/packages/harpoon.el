;; -*- lexical-binding: t; -*-
(use-package harpoon
  :config
  (define-key sp/keys-keymap (kbd "M-RET") 'harpoon-add-file)
  (define-key sp/keys-keymap (kbd "M-<return>") 'harpoon-add-file)
  (define-key sp/keys-keymap (kbd "C-c h RET") 'harpoon-add-file)
  (define-key sp/keys-keymap (kbd "C-c h f") 'harpoon-toggle-file)
  (define-key sp/keys-keymap (kbd "C-c h h") 'harpoon-toggle-quick-menu)
  (define-key sp/keys-keymap (kbd "C-c h c") 'harpoon-clear)
  (define-key sp/keys-keymap (kbd "M-n") 'harpoon-go-to-1)
  (define-key sp/keys-keymap (kbd "M-e") 'harpoon-go-to-2)
  (define-key sp/keys-keymap (kbd "M-o") 'harpoon-go-to-3)
  (define-key sp/keys-keymap (kbd "M-i") 'harpoon-go-to-4)
  (define-key sp/keys-keymap (kbd "M-'") 'harpoon-go-to-5)
  (setq harpoon-project-package  'projectile
        harpoon-separate-by-branch t))
