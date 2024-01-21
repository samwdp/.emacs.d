;; -*- lexical-binding: t; -*-
(use-package emacs-lisp-mode
  :straight nil
  :hook (emacs-lisp-mode . tree-sitter-mode)
  :hook (emacs-lisp-mode . tree-sitter-hl-mode)
  )

(use-package csv-mode
  :straight nil
  :mode "\\.csv\\'"
  :hook (csv-mode . csv-align-mode))

(use-package html-ts-mode
  :straight nil
  :mode ("\\.html\\'" . html-ts-mode)
  :hook (html-ts-mode . lsp-deferred))

(use-package csharp-mode
  :straight nil
  :hook (csharp-ts-mode . lsp-deferred)
  :mode (("\\.cs\\'" . csharp-ts-mode)))

(use-package ob-csharp
  :straight (ob-csharp :type git :host github :repo "samwdp/ob-csharp")
  :config
  (org-babel-do-load-languages 'org-babel-load-languages '((csharp . t))))

(use-package sql
  :straight nil
  :config
  (setq sql-ms-program "sqlcmd"))

(use-package razor-mode
  :straight (razor-mode :type git :host github :repo "samwdp/razor-mode")
  :mode ("\\.razor\\'" . razor-mode)
  :mode ("\\.cshtml\\'" . razor-mode)
  )

(use-package sharper
  :bind ("C-c n" . sharper-main-transient))

(use-package powershell)

(use-package csproj-mode
  :straight (csproj-mode :type git :host github :repo "omajid/csproj-mode")
  :mode ("\\.csproj\\'" . csproj-mode)
  )

(use-package odin-mode
  :straight (odin-mode :type git :host github :repo "samwdp/odin-mode")
  :hook (odin-mode . lsp-deferred)
  :mode ("\\.odin\\'" . odin-mode)
  )

(use-package typescript-mode
  :straight nil
  :hook (typescript-ts-mode . lsp-deferred)
  :mode (("\\.ts\\'" . typescript-ts-mode)
         ("\\.tsx\\'". typescript-ts-mode))
  :custom
  (typescript-indent-level 2)
  (typescript-ts-mode-indent-offset 2))

(use-package sass-mode
  :straight nil
  :hook (sass-mode . lsp-deferred)
  :mode "\\.sass\\'")

(use-package css-mode
  :straight nil
  :hook (css-ts-mode . lsp-deferred)
  :mode (("\\.css\\'" . css-ts-mode)))

(use-package scss-mode
  :straight nil
  :hook (scss-mode . lsp-deferred)
  :mode "\\.scss\\'")

(use-package go-mode
  :straight nil
  :hook (go-ts-mode . lsp-deferred)
  :custom
  (go-ts-mode-indent-offset 4)
  :mode (("\\.go\\'" . go-ts-mode)))

(use-package json-mode
  :straight nil
  :hook (json-ts-mode . lsp-deferred)
  :mode (("\\.json\\'" . json-ts-mode))
  )

(use-package yaml-mode
  :straight nil
  :hook (yaml-ts-mode . lsp-deferred)
  :mode (("\\.yaml" . yaml-ts-mode))
  :mode "Procfile\\'")

(use-package toml-mode
  :straight nil
  :hook (toml-ts-mode . lsp-deferred)
  :mode (("\\.toml" . toml-ts-mode)))

(use-package cc-mode
  :straight nil
  :hook (c-ts-mode . lsp-deferred)
  :mode (("\\.c\\'" . c-ts-mode)
         ("\\.h\\'" . c-ts-mode)))

(use-package rust-mode
  :straight nil
  :hook (rust-ts-mode . lsp-deferred)
  :mode (("\\.rs\\'" . rust-ts-mode)))

(use-package docker
  :defer t)

(use-package dockerfile-mode
  :mode (("\\.docker\\'" . dockerfile-ts-mode)))

(use-package plantuml-mode
  :config
  (setq plantuml-jar-path "c:/tools/plantuml/plantuml.jar")
  (setq plantuml-default-exec-mode 'jar))

(use-package restclient
  :mode ("\\.http\\'" . restclient-mode))

(use-package glsl-mode)
