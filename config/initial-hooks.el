;; -*- lexical-binding: t; -*-
;;; inital hooks
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'minibuffer-setup-hook #'sp/defer-garbage-collection-h)
(add-hook 'minibuffer-exit-hook #'sp/restore-garbage-collection-h)

(add-to-list 'auto-mode-alist '("\\.xls\\'" . no-xls))
(add-to-list 'auto-mode-alist '("\\.xlsx\\'" . no-xls))
