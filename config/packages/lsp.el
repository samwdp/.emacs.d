;; -*- lexical-binding: t; -*-
(when USE-LSP
  (use-package lsp-mode
    :bind (:map lsp-mode-map
                ("C-," . 'lsp-execute-code-action))
    :hook (lsp-mode . lsp-optimization-mode)
    :hook (lsp-mode . lsp-signature-mode)
    :hook (lsp-completion-mode . my/lsp-mode-setup-completion)
    :commands (lsp lsp-deferred)
    :custom
    (lsp-completion-provider :none) ;; we use Corfu!
    :init
    (defun my/lsp-mode-setup-completion ()
      (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
            '(orderless)))
    (advice-add #'lsp-completion-at-point :around #'cape-wrap-noninterruptible)
    (setq lsp-completion-provider nil)
    (setq lsp-keymap-prefix "C-c l")
    (setq lsp-headerline-breadcrumb-icons-enable t
          lsp-headerline-breadcrumb-enable t
          lsp-headerline-breadcrumb-segments '(symbols)
          lsp-idle-delay 0.01
          lsp-lens-enable nil
          lsp-keep-workspace-alive nil
          lsp-modeline-diagnostics-enable nil
          lsp-modeline-code-actions-enable nil
          lsp-enable-which-key-integration t
          lsp-enable-file-watchers nil
          lsp-enable-folding nil
          lsp-enable-text-document-color nil
          lsp-enable-on-type-formatting nil)
    :config
    (add-hook 'lsp-mode-hook #'corfu-lsp-setup)
    (define-key lsp-mode-map [remap xref-find-apropos] #'consult-lsp-symbols)
    (define-key lsp-mode-map [remap lookup-implementation] #'lsp-goto-implementation)
    (define-key lsp-mode-map [remap lookup-declaration] #'lsp-find-declaration)
    (define-key lsp-mode-map [remap lookup-reference] #'lsp-find-references)
    (define-key lsp-mode-map [remap lookup-definition] #'lsp-find-definition)
    (define-key lsp-mode-map [remap lookup-type-definition] #'lsp-goto-type-definition)
    (define-key lsp-mode-map [remap lookup-doc] #'lsp-ui-doc-glance)
    (define-key lsp-mode-map [remap sp/format-buffer] #'lsp-format-buffer)
    (define-key lsp-mode-map (kbd "<f7>") 'lsp-ui-doc-focus-frame)

    (add-to-list 'lsp-language-id-configuration '(odin-mode . "odin"))
    (add-to-list 'lsp-language-id-configuration '(html-ts-mode . "html"))
    (add-to-list 'lsp-language-id-configuration '("\\.razor\\'" . "razor"))

    (lsp-register-client
     (make-lsp-client :new-connection (lsp-stdio-connection "c:/tools/ols/ols.exe")
                      :major-modes '(odin-mode)
                      :server-id 'ols
                      :multi-root t))

    (lsp-register-client
     (make-lsp-client :new-connection (lsp-stdio-connection "D:/projects/htmx-lsp/target/release/htmx-lsp.exe")
                      :major-modes '(html-ts-mode)
                      :server-id 'htmx-lsp
                      :add-on? t
                      :multi-root t))
    ;; (setq lsp-enable-which-key-integration t)
    )

  (use-package lsp-ui
    :hook (lsp-mode . lsp-ui-mode)
    :config
    (defun toggle-doc ()
      (interactive)
      (cursor)
      )
    (setq lsp-ui-doc-enable nil
          lsp-ui-doc-position 'at-point
          lsp-ui-doc-show-with-cursor t
          lsp-signature-auto-activate t
          lsp-signature-render-documentation t
          lsp-ui-sideline-show-code-actions nil
          lsp-ui-sideline-ignore-duplicate t
          lsp-ui-sideline-show-hover nil
          lsp-ui-sideline-show-diagnostics t
          lsp-ui-sideline-actions-icon lsp-ui-sideline-actions-icon-default)
    (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
    (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)

    (evil-define-key 'normal lsp-mode-map (kbd "K") 'lsp-ui-doc-glance)
    (define-key lsp-ui-peek-mode-map (kbd "j") #'lsp-ui-peek--select-next)
    (define-key lsp-ui-peek-mode-map (kbd "k") #'lsp-ui-peek--select-prev)
    (define-key lsp-ui-peek-mode-map (kbd "M-j") #'lsp-ui-peek--select-next-file)
    (define-key lsp-ui-peek-mode-map (kbd "M-j") #'lsp-ui-peek--select-prev-file))

  (use-package consult-lsp
    :defer t)

  (use-package dap-mode
    :bind (:map dap-mode-map
                ("<f10>" . 'dap-next)
                ("<f11>" . 'dap-step-in)
                ("<f5>" . 'dap-continue))
    :commands dap-debug
    :hook (dap-mode . dap-tooltip-mode)
    :config
    (require 'dap-node)
    (require 'dap-chrome)
    (require 'dap-firefox)
    (require 'dap-edge)
    (require 'dap-netcore)
    (require 'dap-lldb)
    (require 'dap-cpptools))

  (use-package dap-ui
    :straight nil
    :after dap-mode
    :hook (dap-mode . dap-ui-mode)
    :hook (dap-ui-mode . dap-ui-controls-mode))

  (use-package lsp-tailwindcss
    :straight (lsp-tailwindcss :type git :host github :repo "merrickluo/lsp-tailwindcss")
    :config
    (setq lsp-tailwindcss-add-on-mode t)
    (add-to-list 'lsp-tailwindcss-major-modes 'html-ts-mode)
    )

  )
