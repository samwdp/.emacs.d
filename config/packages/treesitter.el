(use-package treesit
  :straight nil
  :commands (treesit-install-language-grammar)
  :init
  (setq treesit-font-lock-level 4))

(use-package treesit-auto
  :straight (:host github :repo "renzmann/treesit-auto")
  :config (setq treesit-auto-install 'prompt)
  (global-treesit-auto-mode))
(use-package combobulate
  :straight (:host github :repo "mickeynp/combobulate"))
