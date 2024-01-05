
;;; additional settings
(when IS-WINDOWS (setq package-gnupghome-dir (concat user-emacs-directory "elpa/gnupg/pubring.kbx")))

(when IS-WINDOWS
  (setq backup-directory-alist '(("." . "~/.emacs.d/backup"))))

(when IS-LINUX
  (setq backup-directory-alist '(("." . "~/.config/emacs/backup"))))

(setq  backup-by-copying t    ; Don't delink hardlinks
       version-control t      ; Use version numbers on backups
       delete-old-versions t  ; Automatically delete excess backups
       kept-new-versions 20   ; how many of the newest versions to keep
       kept-old-versions 5    ; and how many of the old
       )

(unless package-archive-contents
  (package-refresh-contents))

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))
(when (daemonp)
  (exec-path-from-shell-initialize))

(if (display-graphic-p)
    (unicode-fonts-setup-h (selected-frame))
  (add-hook 'after-make-frame-functions 'unicode-fonts-setup-h))
