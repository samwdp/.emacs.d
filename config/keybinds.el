;; -*- lexical-binding: t; -*-
(global-set-key (kbd "C-SPC") 'completion-at-point)
(global-set-key (kbd "C-M-n") 'treesit-end-of-defun)
(global-set-key (kbd "C-M-p") 'treesit-beginning-of-defun)
(global-set-key (kbd "C-M-j") 'treesit-forward-sexp)
(global-set-key (kbd "C-p") 'consult-projectile)
(global-set-key [remap lookup-definition] #'xref-find-definitions)
(global-set-key [remap lookup-reference] #'xref-find-references)
(global-set-key [remap sp/format-buffer] #'format-all-buffer)

(use-package windmove
  :config
  (setq windmove-wrap-around t)
  )

(use-package general
  :config
  (general-evil-setup)

  ;; set up 'SPC' as the global leader key
  (general-create-definer sp/leader-keys
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "SPC" ;; set leader
    :global-prefix "C-M-SPC") ;; access leader in insert mode

  (sp/leader-keys
    ;; single use keymaps
    "." '(find-file :wk "find files")
    "SPC" '(consult-projectile :wk "find files")
    "TAB" '(persp-switch :wk "switch project")
    "f" '(sp/format-buffer :wk "format buffer")
    "w" '(save-buffer :wk "save")
    )

  (sp/leader-keys
    ;; inbuilt applications
    "a" '(:ignore t :wk "application")
    "ac" '(quick-calc :wk "quick calc")
    "as" '(global-text-scale-adjust :wk "global text scale")
    )

  (sp/leader-keys
    ;; buffer configuration
    "b" '(:ignore t :wk "buffer")
    "bb" '(consult-project-buffer :wk "Switch buffer")
    "bd" '(kill-this-buffer :wk "Switch buffer")
    "bB" '(consult-buffer :wk "all buffers")
    "bk" '(kill-this-buffer :wk "Kill this buffer")
    "bn" '(next-buffer :wk "Next buffer")
    "bp" '(previous-buffer :wk "Previous buffer")
    "br" '(revert-buffer :wk "Reload buffer")
    )

  (sp/leader-keys
    ;; code actions
    "c" '(:ignore t :wk "code")
    "cc" '(projectile-compile-project :wk "compile")
    "ce" '(consult-flycheck :wk "flycheck")
    "cm" '(consult-flymake :wk "flycheck")
    "ca" '(lsp-execute-code-action :wk "code actions")
    )

  (sp/leader-keys
    ;; kill things
    "d" '(:ignore :wk "window")
    "dw" '(delete-window :wk "delete window")
    )

  (sp/leader-keys
    ;; eval keymaps
    "e" '(:ignore t :wk "eval")
    "eb" '(eval-buffer :wk "eval buffer")
    "ed" '(eval-defun :wk "eval defun")
    "er" '(eval-region :wk "eval region")
    "ei" '((lambda ()
             (interactive)
             (find-file (expand-file-name (concat user-emacs-directory "init.el"))))
           :wk "emacs config")
    )

  (sp/leader-keys
    ;; magit keymaps
    "g" '(:ignore t :wk "git")
    "gs" '(magit-status :wk "magit status")
    )

  (sp/leader-keys
    "h" '(:ignore t :wk "harpoon")
    "hc" '(harpoon-clear :wk "harpoon toggle file")
    "hf" '(harpoon-toggle-file :wk "harpoon toggle file")
    "ht" '(harpoon-add-file :wk "harpoon toggle file")
    )

  (sp/leader-keys
    ;; insert keymaps
    "i" '(:ignore t :wk "insert")
    "is" '(consult-yasnippet t :wk "yasnippet")
    )

  (sp/leader-keys
    ;; open applications
    "o" '(:ignore t :wk "open")
    "oe" '(projectile-run-eshell :wk "eshell")
    "ot"  (if IS-LINUX
              '(projectile-run-vterm :wk "vterm")
            '(projectile-run-eshell :wk "eshell"))
    )

  (sp/leader-keys
    ;; project keymaps
    "p" '(:ignore t :wk "project")
    "pc" '(projectile-compile-project
           t :wk "project")
    "pp" '(consult-projectile-switch-project :wk "switch project") ;; find some way to integrate consult with this automatically
    "pk" '(persp-kill :wk "project kill")
    "pv" '(dired :wk "dired")
    "ps" '(consult-ripgrep :wk "search in project")
    "pr" '(recompile :wk "recompile")
    "pI" '(projectile-invalidate-cache :wk "invalidate cache")
    "pi" '(projectile-discover-projects-in-search-path :wk "invalidate cache")
    )

  (sp/leader-keys
    ;; project keymaps
    "t" '(:ignore t :wk "diagnosticts")
    "tb" '(toggle-big-font :wk "toggle large font")
    "tt" '(consult-lsp-diagnostics :wk "list diagnostics")
    "tp" '(+popup/toggle :wk "toggle popup")
    )

  (sp/leader-keys
    ;; search
    "s" '(:ignore t :wk "search")
    "ss" '( consult-line :wk "find line")
    )
  )
