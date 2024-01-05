;;; functions

(defun no-xls (&optional filename)
  (interactive "File Name: ")
  (when (and filename
             (not (buffer-file-name)))
    (write-file (make-temp-file filename)))
  (erase-buffer)
  (shell-command
   (format "c:\\tools\\xlhtml\\xlhtml.exe -nc -te %s | w3m -dump -T text/html" (buffer-file-name))
   (current-buffer))
  (setq buffer-file-name nil)
  (set-buffer-modified-p nil))

(defun +plantuml-org-babel-execute:plantuml-a (body params)
  "Execute a block of plantuml code with org-babel.
This function is called by `org-babel-execute-src-block'."
  (require 'plantuml-mode)
  ;; REVIEW Refactor me
  (let* ((body (replace-regexp-in-string
                "^[[:blank:]\n]*\\(@start\\)"
                "\\\\\\1"
                body))
         (fullbody (org-babel-plantuml-make-body body params))
         (out-file (or (cdr (assq :file params))
                       (org-babel-temp-file "plantuml-" ".png")))
         (in-file (org-babel-temp-file "plantuml-")))
    (if (eq plantuml-default-exec-mode 'server)
        (if (bound-and-true-p org-export-current-backend)
            (user-error "Exporting plantuml diagrams in server mode is not supported (see `plantuml-default-exec-mode')")
          (save-current-buffer
            (save-match-data
              (with-current-buffer
                  (url-retrieve-synchronously (plantuml-server-encode-url body)
                                              nil t)
                (goto-char (point-min))
                ;; skip the HTTP headers
                (while (not (looking-at "\n")) (forward-line))
                (delete-region (point-min) (+ 1 (point)))
                (write-file out-file)))))
      (let* ((cmd (concat (cond ((eq plantuml-default-exec-mode 'executable)
                                 (unless (executable-find plantuml-executable-path)
                                   (error "Could not find plantuml at %s"
                                          (executable-find plantuml-executable-path)))
                                 (concat (shell-quote-argument (executable-find plantuml-executable-path))
                                         " --headless"))
                                ((not (file-exists-p plantuml-jar-path))
                                 (error "Could not find plantuml.jar at %s" org-plantuml-jar-path))
                                ((concat "java " (cdr (assoc :java params)) " -jar "
                                         (shell-quote-argument
                                          (expand-file-name plantuml-jar-path)))))
                          " "
                          (pcase (file-name-extension out-file)
                            ("png" "-tpng")
                            ("svg" "-tsvg")
                            ("eps" "-teps")
                            ("pdf" "-tpdf")
                            ("tex" "-tlatex")
                            ("vdx" "-tvdx")
                            ("xmi" "-txmi")
                            ("scxml" "-tscxml")
                            ("html" "-thtml")
                            ("txt" "-ttxt")
                            ("utxt" "-utxt"))
                          " "
                          " -p " (cdr (assoc :cmdline params)) " < "
                          (org-babel-process-file-name in-file)
                          " > "
                          (org-babel-process-file-name out-file))))
        (with-temp-file in-file (insert fullbody))
        (message "%s" cmd)
        (org-babel-eval cmd "")))
    (unless (cdr (assq :file params))
      out-file)))
;; (require 'ob-plantuml)
(with-eval-after-load 'org
  (advice-add #'org-babel-execute:plantuml
              :override #'+plantuml-org-babel-execute:plantuml-a)

  (org-babel-do-load-languages 'org-babel-load-languages '((ruby . t)
                                                           (plantuml . t)
                                                           (emacs-lisp . t))))
(cl-defun slot/vc-install (&key (fetcher "github") (extension "com") repo name rev backend)
  "Install a package from a remote if it's not already installed.
This is a thin wrapper around `package-vc-install' in order to
make non-interactive usage more ergonomic.  Takes the following
named arguments:

- FETCHER the remote where to get the package (e.g., \"gitlab\").
  If omitted, this defaults to \"github\".

- REPO should be the name of the repository (e.g.,
  \"slotThe/arXiv-citation\".

- NAME, REV, and BACKEND are as in `package-vc-install' (which
  see)."
  (let* ((url (format "https://www.%s.%s/%s" fetcher extension repo))
         (iname (when name (intern name)))
         (pac-name (or iname (intern (file-name-base repo)))))
    (unless (package-installed-p pac-name)
      (package-vc-install url iname rev backend))))


(defun sp/defer-garbage-collection-h ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun sp/restore-garbage-collection-h ()
  ;; Defer it so that commands launched immediately after will enjoy the
  ;; benefits.
  (run-at-time
   1 nil (lambda () (setq gc-cons-threshold 16777216))))



;; (server-start)
(defun efs/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                    (time-subtract after-init-time before-init-time)))
           gcs-done))


(defun sp/new-frame ()
  (set-face-attribute 'default nil :font (font-spec :family sp/font-string :size sp/text-height))
  (set-face-attribute 'fixed-pitch nil :font (font-spec :family sp/font-string :size sp/text-height))
  (set-frame-parameter (selected-frame) 'alpha-background 1.0 ))

(defun lookup-definition ())
(defun lookup-reference ())
(defun lookup-implementation ())
(defun lookup-declaration ())
(defun lookup-type-definition ())
(defun lookup-doc ())
(defun sp/format-buffer ())

(defun toggle-transparency () )

(defun transparency (value)
  "Sets the transparency of the frame window. 0=transparent/1.0=opaque"
  (interactive "nTransparency Value 0 - 1 opaque: t")
  (set-frame-parameter (selected-frame) 'alpha-background value))


(defun unicode-fonts-setup-h (frame)
  "Run unicode-fonts-setup, then remove the hook."
  (when (and frame (display-graphic-p frame))
    (with-selected-frame frame
      (require 'unicode-fonts)
      (sp/new-frame)
      (unicode-fonts-setup))))


(defun spawn-shell (name)
  (interactive "MName of new shell: ")
  (pop-to-buffer (get-buffer-create (generate-new-buffer-name name)))
  (term (current-buffer)))

(defun eshell-with-name ()
  (interactive)
  (let* ((eshell-buffer-names (mapcar (lambda (buf)
					                    (buffer-name buf))
					                  (buffer-list)))
	     (match (completing-read "eshell buffers: "
				                 eshell-buffer-names
                                 (lambda (buf)
				                   (string-match-p "*eshell*" buf))))
	     (eshell-buffer-exists (member match eshell-buffer-names)))
    (if eshell-buffer-exists
	    (switch-to-buffer match)
	  (eshell 99)
	  (rename-buffer (concat "*eshell*<" match ">")))))

(defun sp/evil-yank-advice (orig-fn beg end &rest args)
  (pulse-momentary-highlight-region beg end)
  (apply orig-fn beg end args))

(defun +treemacs/toggle ()
  "Initialize or toggle treemacs.
Ensures that only the current project is present and all other projects have
been removed.
Use `treemacs' command for old functionality."
  (interactive)
  (require 'treemacs)
  (pcase (treemacs-current-visibility)
    (`visible (delete-window (treemacs-get-local-window)))
    (_ (if (sp/project-p)
           (treemacs-add-and-display-current-project)
         (treemacs)))))

(defun sp/project-p (&optional dir)
  "Return t if DIR (defaults to `default-directory') is a valid project."
  (and (sp/project-root dir)
       t))

(defun sp/project-root (&optional dir)
  "Return the project root of DIR (defaults to `default-directory').
Returns nil if not in a project."
  (let ((projectile-project-root
         (unless dir (bound-and-true-p projectile-project-root)))
        projectile-require-project-root)
    (projectile-project-root dir)))

;;;###autoload
(defun +lsp/switch-client (client)
  "Switch to another LSP server."
  (interactive
   (progn
     (require 'lsp-mode)
     (list (completing-read
            "Select server: "
            (or (mapcar #'lsp--client-server-id (lsp--filter-clients (-andfn #'lsp--supports-buffer?
                                                                             #'lsp--server-binary-present?)))
                (user-error "No available LSP clients for %S" major-mode))))))
  (require 'lsp-mode)
  (let* ((client (if (symbolp client) client (intern client)))
         (match (car (lsp--filter-clients (lambda (c) (eq (lsp--client-server-id c) client)))))
         (workspaces (lsp-workspaces)))
    (unless match
      (user-error "Couldn't find an LSP client named %S" client))
    (let ((old-priority (lsp--client-priority match)))
      (setf (lsp--client-priority match) 9999)
      (unwind-protect
          (if workspaces
              (lsp-workspace-restart
               (if (cdr workspaces)
                   (lsp--completing-read "Select server: "
                                         workspaces
                                         'lsp--workspace-print
                                         nil t)
                 (car workspaces)))
            (lsp-mode +1))
        (add-transient-hook! 'lsp-after-initialize-hook
                             (setf (lsp--client-priority match) old-priority))))))
;;;###autoload
(defun projectile-persp-switch-project (project-to-switch)
  "Switch to a project or perspective we have visited before.
If the perspective of corresponding project does not exist, this
function will call `persp-switch' to create one and switch to
that before `projectile-switch-project' invokes
`projectile-switch-project-action'.

Otherwise, this function calls `persp-switch' to switch to an
existing perspective of the project unless we're already in that
perspective."
  (interactive (list (projectile-completing-read "Switch to project: "
                                                 (projectile-relevant-known-projects))))
  (let* ((name (or projectile-project-name
                   (funcall projectile-project-name-function project-to-switch)))
         (persp (gethash name (perspectives-hash))))
    (cond
     ;; project-specific perspective already exists
     ((and persp (not (equal persp (persp-curr))))
      (persp-switch name))
     ;; persp exists but not match with projectile-name
     ((and persp (not (equal persp name)))
      (persp-switch name)
      (projectile-switch-project-by-name project-to-switch))
     ;; project-specific perspective doesn't exist
     ((not persp)
      (let ((frame (selected-frame)))
        (persp-switch name)
        (projectile-switch-project-by-name project-to-switch)
        ;; Clean up if we switched to a new frame. `helm' for one allows finding
        ;; files in new frames so this is a real possibility.
        (when (not (equal frame (selected-frame)))
          (with-selected-frame frame
            (persp-kill name)))))
     )))

(defun load-directory (directory)
  "Load recursively all `.el' files in DIRECTORY."
  (dolist (element (directory-files-and-attributes directory nil nil nil))
    (let* ((path (car element))
           (fullpath (concat directory "/" path))
           (isdir (car (cdr element)))
           (ignore-dir (or (string= path ".") (string= path ".."))))
      (cond
       ((and (eq isdir t) (not ignore-dir))
        (load-directory fullpath))
       ((and (eq isdir nil) (string= (substring path -3) ".el"))
        (load (file-name-sans-extension fullpath)))))))

;;;###autoload
(defun +org/table-previous-row ()
  "Go to the previous row (same column) in the current table. Before doing so,
re-align the table if necessary. (Necessary because org-mode has a
`org-table-next-row', but not `org-table-previous-row')"
  (interactive)
  (org-table-maybe-eval-formula)
  (org-table-maybe-recalculate-line)
  (if (and org-table-automatic-realign
           org-table-may-need-update)
      (org-table-align))
  (let ((col (org-table-current-column)))
    (beginning-of-line 0)
    (when (or (not (org-at-table-p)) (org-at-table-hline-p))
      (beginning-of-line))
    (org-table-goto-column col)
    (skip-chars-backward "^|\n\r")
    (when (org-looking-at-p " ")
      (forward-char))))


;;
;;; Row/Column insertion

;;;###autoload
(defun +org/table-insert-column-left ()
  "Insert a new column left of the current column."
  (interactive)
  (org-table-insert-column)
  (org-table-move-column-left))

;;;###autoload
(defun +org/table-insert-row-below ()
  "Insert a new row below the current row."
  (interactive)
  (org-table-insert-row 'below))


;;
;;; Hooks

;;;###autoload
(defun +org-realign-table-maybe-h ()
  "Auto-align table under cursor."
  (when (and (org-at-table-p) org-table-may-need-update)
    (let ((pt (point))
          (inhibit-message t))
      (if org-table-may-need-update (org-table-align))
      (goto-char pt))))

;;;###autoload
(defun +org-enable-auto-reformat-tables-h ()
  (message "Hooks")
  (add-hook 'meow-insert-state-exit-hook #'+org-realign-table-maybe-h nil t))

;;;###autoload
(defun +org-delete-backward-char-and-realign-table-maybe-h ()
  "Ensure deleting characters with backspace doesn't deform the table cell."
  (when (eq major-mode 'org-mode)
    (org-check-before-invisible-edit 'delete-backward)
    (save-match-data
      (when (and (org-at-table-p)
                 (not (org-region-active-p))
                 (string-match-p "|" (buffer-substring (point-at-bol) (point)))
                 (looking-at-p ".*?|"))
        (let ((pos (point))
              (noalign (looking-at-p "[^|\n\r]*  |"))
              (c org-table-may-need-update))
          (delete-char -1)
          (unless overwrite-mode
            (skip-chars-forward "^|")
            (insert " ")
            (goto-char (1- pos)))
          ;; noalign: if there were two spaces at the end, this field
          ;; does not determine the width of the column.
          (when noalign (setq org-table-may-need-update c)))
        t))))


;;
;;; Advice

;;;###autoload
(defun +org-realign-table-maybe-a (&rest _)
  "Auto-align table under cursor and re-calculate formulas."
  (when (eq major-mode 'org-mode)
    (+org-realign-table-maybe-h)))