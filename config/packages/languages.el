
(use-package csv-mode
  :mode "\\.csv\\'"
  :hook (csv-mode . csv-align-mode))

(use-package html-ts-mode
  :hook (html-ts-mode . lsp-deferred))
(use-package csharp-mode
  :hook (csharp-ts-mode . lsp-deferred)
  :mode (("\\.cs\\'" . csharp-ts-mode)))

(use-package ob-csharp
  :init (slot/vc-install :fetcher "github" :repo "samwdp/ob-csharp")
  :config
  (org-babel-do-load-languages 'org-babel-load-languages '((csharp . t))))

(use-package sql
  :ensure nil
  :config
  (setq sql-ms-program "sqlcmd"))

(use-package razor-mode
  :init (slot/vc-install :fetcher "github" :repo "samwdp/razor-mode")
  :mode ("\\.razor\\'" . razor-mode)
  :mode ("\\.cshtml\\'" . yas--direct-razor-mode))

(use-package sharper
  :bind ("C-c n" . sharper-main-transient))

(use-package powershell)

(use-package csproj-mode
  :init (slot/vc-install :fetcher "github" :repo "omajid/csproj-mode")
  :mode "\\.csproj\\'")

(use-package odin-mode
  :hook (odin-mode . lsp-deferred)
  :init (slot/vc-install :fetcher "github" :repo "samwdp/odin-mode")
  :mode "\\.odin\\'")

(use-package typescript-mode
  :hook (typescript-ts-mode . lsp-deferred)
  :mode (("\\.ts\\'" . typescript-ts-mode)
         ("\\.tsx\\'". typescript-ts-mode))
  :custom
  (typescript-indent-level 2)
  (typescript-ts-mode-indent-offset 2))

(use-package sass-mode
  :hook (sass-mode . lsp-deferred)
  :mode "\\.sass\\'")

(use-package css-mode
  :hook (css-ts-mode . lsp-deferred)
  :mode (("\\.css\\'" . css-ts-mode)))

(use-package scss-mode
  :hook (scss-mode . lsp-deferred)
  :mode "\\.scss\\'")

(use-package go-mode
  :hook (go-ts-mode . lsp-deferred)
  :custom
  (go-ts-mode-indent-offset 4)
  :mode (("\\.go\\'" . go-ts-mode)))

(use-package json-mode
  :hook (json-ts-mode . lsp-deferred)
  :mode (("\\.json\\'" . json-ts-mode))
  )

(use-package yaml-mode
  :hook (yaml-ts-mode . lsp-deferred)
  :mode (("\\.yaml" . yaml-ts-mode))
  :mode "Procfile\\'")

(use-package toml-mode
  :hook (toml-ts-mode . lsp-deferred)
  :mode (("\\.toml" . toml-ts-mode)))

(use-package cc-mode
  :hook (c-ts-mode . lsp-deferred)
  :mode (("\\.c\\'" . c-ts-mode)
         ("\\.h\\'" . c-ts-mode)))

(use-package rust-mode
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
