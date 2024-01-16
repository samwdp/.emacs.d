(when IS-LINUX
  (use-package vterm
    :bind (:map vterm-mode-map
                ("C-l" . windmove-right)
                ("C-j" . windmove-down)
                ("C-k" . windmove-up)
                ("C-h" . windmove-left)
                )))

(use-package eshell
  :straight nil
  :bind (:map eshell-mode-map
              ("C-M-k" . windmove-up)
              ("C-M-l" . popper-cycle)
              ("C-k" . windmove-up)
              ("C-j" . windmove-down)
              ("C-M-j" . windmove-down))
  )

(use-package eat
  :bind (:map eat-mode-map
              ("C-l" . windmove-right)
              ("C-j" . windmove-down)
              ("C-k" . windmove-up)
              ("C-h" . windmove-left)))

(use-package shell-switcher)
