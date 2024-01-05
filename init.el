(setq gc-cons-thershold 100000000)
(setq read-process-output-max (* 1024 1024)) ;; 1mb

(load-file (concat user-emacs-directory "config/sets.el"))
(load-file (concat user-emacs-directory "config/custom-functions.el"))
(load-file (concat user-emacs-directory "config/keybinds.el"))
(load-file (concat user-emacs-directory "config/additional-settings.el"))
(load-file (concat user-emacs-directory "config/initial-hooks.el"))
(load-directory (concat user-emacs-directory "config/packages"))
(load-file (concat user-emacs-directory "config/minor-modes.el"))

(setq gc-cons-thershold (* 2 1000 1000))
