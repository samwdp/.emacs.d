(when IS-LINUX
  (use-package vterm
    :config
    (define-key vterm-mode-map (kbd "C-l") 'windmove-right)
    (define-key vterm-mode-map (kbd "C-j") 'windmove-down)
    (define-key vterm-mode-map (kbd "C-k") 'windmove-up)
    (define-key vterm-mode-map (kbd "C-h") 'windmove-left)
    ))

(use-package eshell
  :straight nil
  :hook (eshell-mode . eat-eshell-mode)
  :hook (eshell-mode . eat-eshell-visual-command-mode))

(use-package eat
  :config
  (define-key eat-mode-map (kbd "C-l") 'windmove-right)
  (define-key eat-mode-map (kbd "C-j") 'windmove-down)
  (define-key eat-mode-map (kbd "C-k") 'windmove-up)
  (define-key eat-mode-map (kbd "C-h") 'windmove-left)
  )
