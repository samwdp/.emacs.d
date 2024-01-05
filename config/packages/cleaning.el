(use-package gcmh
  :ensure t
  :config
  (gcmh-mode 1))


(use-package no-littering
  :ensure t
  :config
  (setq auto-save-file-name-transforms
	    `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))
