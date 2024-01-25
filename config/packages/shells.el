;; -*- lexical-binding: t; -*-
(when IS-LINUX
  (use-package vterm
    :bind (:map vterm-mode-map
                ("C-l" . windmove-right)
                ("C-j" . windmove-down)
                ("C-k" . windmove-up)
                ("C-h" . windmove-left))))

(defun sp/configure-eshell ()
  (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)
  (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)

  (evil-collection-define-key 'normal 'eshell-mode-map (kbd "C-k") nil)
  (evil-collection-define-key 'normal 'eshell-mode-map (kbd "C-j") nil)
  (evil-normalize-keymaps)

  (setq eshell-history-size 10000
        eshell-buffer-maximum-lines 10000
        eshell-hist-ignoredups t
        eshell-scroll-to-bottom-on-input t)
  )
(use-package eshell-git-prompt)

;;;###autoload
(defface +eshell-prompt-pwd '((t (:inherit font-lock-constant-face)))
  "TODO"
  :group 'eshell)

;;;###autoload
(defface +eshell-prompt-git-branch '((t (:inherit font-lock-builtin-face)))
  "TODO"
  :group 'eshell)


;;;###autoload
(defun doom-call-process (command &rest args)
  "Execute COMMAND with ARGS synchronously.

Returns (STATUS . OUTPUT) when it is done, where STATUS is the returned error
code of the process and OUTPUT is its stdout output."
  (with-temp-buffer
    (cons (or (apply #'call-process command nil t nil (remq nil args))
              -1)
          (string-trim (buffer-string)))))

(defun +eshell--current-git-branch ()
  ;; TODO Refactor me
  (cl-destructuring-bind (status . output)
      (doom-call-process "git" "symbolic-ref" "-q" "--short" "HEAD")
    (if (equal status 0)
        (format " [%s]" output)
      (cl-destructuring-bind (status . output)
          (doom-call-process "git" "describe" "--all" "--always" "HEAD")
        (if (equal status 0)
            (format " [%s]" output)
          "")))))

;;;###autoload
(defun +eshell-default-prompt-fn ()
  "Generate the prompt string for eshell. Use for `eshell-prompt-function'."
  (require 'shrink-path)
  (concat (if (bobp) "" "\n")
          (let ((pwd (eshell/pwd)))
            (propertize (if (equal pwd "~")
                            pwd
                          (abbreviate-file-name (shrink-path-file pwd)))
                        'face '+eshell-prompt-pwd))
          (propertize (+eshell--current-git-branch)
                      'face '+eshell-prompt-git-branch)
          (propertize " λ" 'face (if (zerop eshell-last-command-status) 'success 'error))
          " "))

(use-package eshell
  :straight nil
  :hook (eshell-first-time-mode . sp/configure-eshell)
  :init

  (setq eshell-aliases-file (concat user-emacs-directory "eshell/aliases")
        eshell-rc-script (concat user-emacs-directory "eshell/profile")
        eshell-login-script (concat user-emacs-directory "eshell/login"))
  :config
  (require 'eshell-git-prompt)
  (evil-collection-define-key 'normal 'eshell-mode-map (kbd "C-k") nil)
  (evil-collection-define-key 'normal 'eshell-mode-map (kbd "C-j") nil)
  (unbind-key (kbd "C-j") eshell-mode-map)
  (unbind-key (kbd "C-k") eshell-mode-map)
  (unbind-key (kbd "C-h") eshell-mode-map)
  (unbind-key (kbd "C-l") eshell-mode-map)
  (unbind-key (kbd "C-j") eshell-prompt-mode-map)
  (unbind-key (kbd "C-k") eshell-prompt-mode-map)
  (unbind-key (kbd "C-h") eshell-prompt-mode-map)
  (unbind-key (kbd "C-l") eshell-prompt-mode-map)

  (setq eshell-prompt-regexp "^[^#$\n]* [#$λ] "
        eshell-prompt-function #'+eshell-default-prompt-fn)
  ;; (eshell-git-prompt-use-theme 'git-radar)
  ;; (setq eshell-prompt-regexp "^[^$\n]*\\\$ ")
  )

(use-package eshell-up)

(use-package eshell-syntax-highlighting
  :hook (eshell-mode . eshell-syntax-highlighting-mode))

(use-package eshell-prompt-extras
  :config
  (setq eshell-highlight-prompt nil
        eshell-promtp-function 'epe-theme-multiline-with-status))

(use-package eat
  :bind (:map eat-mode-map
              ("C-l" . windmove-right)
              ("C-j" . windmove-down)
              ("C-k" . windmove-up)
              ("C-h" . windmove-left)))

(use-package shell-switcher)
