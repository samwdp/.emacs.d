(use-package flycheck
  :commands flycheck-list-errors flycheck-buffer
  :config
  (setq flycheck-display-errors-function 'ignore)
  (setq flycheck-checker-error-threshold 10000)
  (setq flycheck-emacs-lisp-load-path 'inherit)
  (delq 'new-line flycheck-check-syntax-automatically)
  (setq flycheck-idle-change-delay 1.0)
  (setq flycheck-buffer-switch-check-intermediate-buffers t)
  (setq flycheck-display-errors-delay 0.25))

(use-package flycheck-tip)
