(setq gc-cons-thershold 100000000)
(setq read-process-output-max (* 1024 1024)) ;; 1mb

(load-file (concat user-emacs-directory "config/sets.el"))
(load-file (concat user-emacs-directory "config/functions.el"))
(load-file (concat user-emacs-directory "config/minor-modes.el"))
(load-file (concat user-emacs-directory "config/keybinds.el"))
(load-file (concat user-emacs-directory "config/additional-settings.el"))
(load-file (concat user-emacs-directory "config/initial-hooks.el"))
(load-file (concat user-emacs-directory "config/packages.el"))

;; (setq gc-cons-thershold (* 2 1000 1000))
