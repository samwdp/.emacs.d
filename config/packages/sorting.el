(use-package marginalia
  :after vertico
  :ensure t
  :custom
  (marginalia-max-relative-age 0)
  (marginalia-align 'right)
  :init
  (marginalia-mode))

(use-package prescient)

(use-package corfu
  :after no-littering
  :bind (:map corfu-map
              ("TAB" . nil)
              ("<tab>" . nil))
  :custom
  (corfu-auto t)
  (corfu-cycle t)
  (corfu-auto-prefix 2)
  (corfu-popupinfo-delay 0.0)
  (corfu-auto-delay 0.01)
  (corfu-quit-at-boundary 'separator)
  :init
  (global-corfu-mode)
  (corfu-history-mode)
  (corfu-popupinfo-mode))

(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-tex))


(use-package vertico
  :config
  (setq vertico-resize nil
        vertico-count 17
        vertico-cycle t
        completion-in-region-function
        (lambda (&rest args)
          (apply (if vertico-mode
                     #'consult-completion-in-region
                   #'completion--in-region)
                 args)))
  :init
  (vertico-mode))

(use-package vertico-directory
  :straight nil
  :after vertico
  )

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; when in a gui frame
(use-package vertico-posframe
  :after vertico
  :config
  (setq vertico-posframe-parameters
        '((left-fringe . 4)
          (right-fringe . 0)
          (alpha-background . 1.0)))
  (setq vertico-posframe-border-width 2)
  (setq vertico-posframe-poshandler #'posframe-poshandler-frame-center)
  (if (display-graphic-p)
      (vertico-posframe-mode +1))
  )

(use-package embark
  :defer t)

(use-package consult
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)
  (advice-add #'register-preview :override #'consult-register-window)
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :config
  (setq consult-preview-key "M-.")
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   :preview-key '(:debounce 0.4 any))

  (setq consult-narrow-key "<"))

(use-package embark-consult
  :after (embark consult))

(use-package consult-dir)

(use-package consult-flycheck)

(use-package savehist
  :init
  (savehist-mode))
