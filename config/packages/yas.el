;; -*- lexical-binding: t; -*-
(use-package yasnippet
  :defer t
  :init
  (yas-global-mode 1))

(use-package auto-yasnippet
  :defer t)

(use-package consult-yasnippet)

(use-package yasnippet-snippets)

(use-package competitive-programming-snippets)

(use-package yasnippet-capf
  :after cape
  :straight (yasnippet-capf :fetcher github :repo "elken/yasnippet-capf")
  :config
  (add-to-list 'completion-at-point-functions #'yasnippet-capf)
  )
