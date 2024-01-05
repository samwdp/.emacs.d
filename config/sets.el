;;; constants
(defconst NATIVECOMP (if (fboundp 'native-comp-available-p) (native-comp-available-p)))
(defconst IS-MAC     (eq system-type 'darwin))
(defconst IS-LINUX   (eq system-type 'gnu/linux))
(defconst IS-WINDOWS (memq system-type '(cygwin windows-nt ms-dos)))
(defconst USE-LSP t)

(setq scroll-margin 8
      idle-update-delay 0.02
      grep-command "grep --color=auto -nHr --null -e "
      visible-bell t
      create-lockfiles nil
      use-short-answers t
      ring-bell-function nil
      make-backup-files t
      backup-by-copying t
      inhibit-startup-message t
      inhibit-compacting-font-caches t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t
      mode-line-end-spaces nil
      set-language-environment "UTF-8")

(setq-default indent-tabs-mode nil
              tab-width 4
              fill-column 80)
(setq-default tab-always-indent nil)
(setq-default display-line-numbers-type 'relative)
(setq custom-theme-directory (concat user-emacs-directory "themes/"))

;;; variables
(defvar sp/text-height 20)
(defvar sp/font-string "FiraCode Nerd Font")

(global-display-fill-column-indicator-mode)
;;; emacs settings
(toggle-frame-maximized)
(blink-cursor-mode 0)
(pixel-scroll-mode 1)
(display-time-mode 1)
(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room
(global-visual-line-mode)
(menu-bar-mode -1)            ; Disable the menu bar


(when NATIVECOMP
  (setq native-comp-async-report-warnings-errors nil)
  (add-to-list 'native-comp-eln-load-path (expand-file-name "eln-cache/" user-emacs-directory)))

(setq emacs-version-short (replace-regexp-in-string
                           "\\([0-9]+\\)\\.\\([0-9]+\\).*"
                           "\\1_\\2" emacs-version))
(setq custom-file (expand-file-name
                   (concat "custom_" emacs-version-short ".el")
                   user-emacs-directory))

;; (package-initialize)
(setq use-package-always-ensure nil)
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

(defvar project-dirs)
(when IS-WINDOWS
  (setq project-dirs '(("D:/work" . 3)
                       ("D:/projects" . 3))))
(when IS-LINUX
  (setq project-dirs '(("~/work/" . 3)
                       ("~/projects/" . 3))))
