(use-package org
  :hook ((org-mode . org-fancy-priorities-mode))
  :config
  ;; (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
  ;; (org-babel-do-load-languages
  ;; 'org-babel-load-languages
  ;; '((plantuml . t)))
  ;; (org-babel-do-load-languages 'org-babel-load-languages '((plantuml . t)
  ;;                                                          (emacs-lisp . t)))
  (setq org-plantuml-jar-path "c:/tools/plantuml/plantuml.jar")
  (setq org-return-follows-link nil)
  (setq org-startup-with-inline-images t)
  (setq org-superstar-special-todo-items t)
  (setq org-display-inline-images t)
  (setq org-ellipsis " ▼")
  (setq org-todo-keywords
        '((sequence
           "TODO(t)"  ; A task that needs doing & is ready to do
           "PROJ(p)"  ; A project, which usually contains other tasks
           "LOOP(r)"  ; A recurring task
           "STRT(s)"  ; A task that is in progress
           "WAIT(w)"  ; Something external is holding up this task
           "HOLD(h)"  ; This task is paused/on hold because of me
           "IDEA(i)"  ; An unconfirmed and unapproved task or notion
           "|"
           "DONE(d)"  ; Task successfully completed
           "KILL(k)") ; Task was cancelled, aborted or is no longer applicable
          (sequence
           "[ ](T)"   ; A task that needs doing
           "[-](S)"   ; Task is in progress
           "[?](W)"   ; Task is being held up or paused
           "|"
           "[X](D)")  ; Task was completed
          (sequence
           "|"
           "OKAY(o)"
           "YES(y)"
           "NO(n)"))
        org-todo-keyword-faces
        '(("[-]"  . +org-todo-active)
          ("STRT" . +org-todo-active)
          ("[?]"  . +org-todo-onhold)
          ("WAIT" . +org-todo-onhold)
          ("HOLD" . +org-todo-onhold)
          ("PROJ" . +org-todo-project)
          ("NO"   . +org-todo-cancel)
          ("KILL" . +org-todo-cancel))))


(use-package org-modern
  :hook((org-mode . org-modern-mode)
        (org-agenda-finilize . org-modern-agenda)))

(use-package org-appear
  :straight (org-appear :type git :fetcher github :repo "awth13/org-appear")
  :hook (org-mode . org-appear-mode)
  :config
  (setq org-appear-autolinks t
        org-appear-autoemphasis t
        org-appear-autoentities t
        org-appear-autokeywords t
        org-appear-autosubmarkers t))


(use-package toc-org
  :hook (org-mode . toc-org-mode))

(use-package org-fancy-priorities
  :hook ((org-mode org-agenda-mode) . org-fancy-priorities-mode))

(use-package org-brain
  :config
  (setq org-brain-path "~/brain"))

(use-package polymode
  :hook (org-brain-visualize-mode . org-brain-polymode))

(use-package evil-org
  :hook (org-mode . evil-org-mode))
