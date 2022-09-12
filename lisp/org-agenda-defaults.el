;;; org-agenda-defaults.el --- Org Agenda setup  -*- lexical-binding: t -*-

(use-package org-agenda
  :straight nil
  :defer t
  :config      ;; formatting for properties
  (setq org-property-format "%-24s %s")
  ;; setup todo keywords
  (setq
   org-todo-keywords
   '((sequence "TODO(t)" "EN_COURS(n)" "|" "FINI(d!)")
     (sequence "ATTENDRE(w@/!)"
	       "ENATTENTE(h@/!)"
	       "|"
	       "ANNULÉ(c@/!)"
	       "RÉUNION(m)")))
  (setq org-todo-keyword-faces
	'(("TODO"     . org-todo)
	  ("ENATTENTE"  . (:foreground "black" :background "#FFEF9F"))
	  ("ANNULÉ" . (:foreground "#94BFF3" :weight bold :strike-through t))
	  ("FINI"     . (:foreground "black" :background "#91ba31"))
	  ("RÉUNION" . '(bold org-todo))
	  ("ATTENDRE" . '(bold shadow))))
  (setq org-use-fast-todo-selection 'expert)
  (setq org-priority-faces
	'((?A . '(bold org-priority))
	  (?B . org-priority)
	  (?C . '(shadow org-priority))))
  (setq org-fontify-done-headline nil)
  (setq org-fontify-todo-headline nil)
  (setq org-fontify-quote-and-verse-blocks t)
  (setq org-fontify-whole-heading-line nil)
  (setq org-fontify-whole-block-delimiter-line nil)
  (setq org-highlight-latex-and-related nil) ; other options affect elisp regexp in src blocks
  (setq org-enforce-todo-dependencies t)
  (setq org-enforce-todo-checkbox-dependencies t)
  (setq org-track-ordered-property-with-tag t)
  (setq org-highest-priority ?A)
  (setq org-lowest-priority ?C)
  (setq org-default-priority ?A)

  (setq org-todo-keywords
	'((sequence
	   "TODO(t)"  ; A task that needs doing & is ready to do
	   ;;  "PROJ(p)"  ; A project, which usually contains other tasks
	   "EN_COURS(s)"  ; A task that is in progress
	   "ATTE(w)"  ; Something external is holding up this task
	   "SUSP(h)"  ; This task is paused/on hold because of me
	   "|"
	   "FINI(d)"  ; Task successfully completed
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
	'(("[ ]"   . org-todo-keyword-todo)
	  ("SUIV" .  org-todo-keyword-next)
	  ("[-]"  .  org-todo-keyword-next)
	  ("[?]"  .  org-todo-keyword-wait)
	  ("ATTE" .  org-todo-keyword-wait)
	  ("SUSP" .  org-todo-keyword-wait)
	  ("PROJ" .  org-todo-keyword-proj)
	  ("KILL" .  org-todo-keyword-kill)
	  ("FINI" .  org-todo-keyword-done)
	  ("[X]"  .  org-todo-keyword-done)
	  ))

  (setq org-agenda-custom-commands
	(quote
         (
          ("n" "Prochaines Tâches"
	   ((agenda "" ((org-deadline-warning-days 7)))
	    (todo "SUIV"
		  ((org-agenda-overriding-header "Prochaines Tâches")))))

	  ;; Low-effort next actions
	  ("l" tags-todo "+TODO=\"EN_COURS\"+Effort<15&+Effort>0"
	   ((org-agenda-overriding-header "Tâches à faible Effort")
	    (org-agenda-max-todos 20)
	    (org-agenda-files org-agenda-files)))
	  ("h" "Habitudes" tags-todo "STYLE=\"habit\""
	   ((org-agenda-overriding-header "Habitudes")
	    (org-agenda-sorting-strategy
	     '(todo-state-down priority-down category-keep))))
	  ("e" "Eisenhower Matrix"
	   ((agenda
	     ""
	     ((org-agenda-overriding-header "Calendrier Eisenhower:")
	      (org-agenda-show-log t)
	      (org-agenda-log-mode-items '(clock state))
	      (org-agenda-category-filter-preset '("-Habitudes"))
	      (org-agenda-span 5)
	      (org-agenda-start-on-weekday t)
	      ;;            (org-agenda-ndays 5)
	      ;;            (org-agenda-start-day "-2d")
	      (org-deadline-warning-days 30)))
	    (tags-todo
	     "+important+urgent/!FINI"
	     ((org-agenda-overriding-header "Tâches importantes et urgentes")
	      (org-tags-match-list-sublevels nil)))
	    (tags-todo  "+important-urgent"
			((org-agenda-overriding-header "Tâches importantes mais non urgentes")
			 (org-tags-match-list-sublevels nil)))
	    (tags-todo "-important+urgent"
		       ((org-agenda-overriding-header "Tâches urgentes mais sans importance")
			(org-tags-match-list-sublevels nil)))
	    (tags-todo "-important-urgent/!TODO"
		       ((org-agenda-overriding-header "Tâches non importantes ni urgentes")
			(org-agenda-category-filter-preset '("-Habitudes"))
			(org-tags-match-list-sublevels nil)))
	    (tags-todo "values"
		       ((org-agenda-overriding-header "Valeurs")
			(org-tags-match-list-sublevels nil)))
	    ))
	  (" " "Agenda"
	   ((agenda ""
		    ((org-agenda-overriding-header "Calendrier d'aujourd'hui:")
		     (org-agenda-show-log t)
		     (org-agenda-log-mode-items '(clock state))
		     ;; (org-agenda-files '(,(add-path-to-org/ "inbox.org")))
		     (org-agenda-text-search-extra-files nil)
		     ;;   (org-agenda-span 'day)
		     ;;   (org-agenda-ndays 3)
		     (org-agenda-start-on-weekday nil)
		     (org-agenda-start-day "-d")
		     (org-agenda-todo-ignore-deadlines nil)))
	    (tags-todo "+important"
		       ((org-agenda-overriding-header "Tâches Importantes à Venir")
			(org-tags-match-list-sublevels nil)))
	    (tags-todo "-important/TODO"
		       ((org-agenda-overriding-header "Tâches de Travail")
			(org-agenda-category-filter-preset '("-Habitudes"))
			(org-agenda-sorting-strategy
			 '(todo-state-down priority-down))))
	    (tags-todo "-important-urgent/TODO"
		       ((org-agenda-overriding-header "Habitudes")
			(org-agenda-sorting-strategy
			 '(todo-state-down priority-down))))
	    (tags "FINI"
		  ((org-agenda-overriding-header "Tâches à la Représenter")
		   (org-tags-match-list-sublevels nil)))))
	  ))))

(use-package org-clock
  :straight nil
  :defer t
  :commands (org-clock-save)
  :init
  (setq
   org-clock-persist-file (expand-file-name "org-clock-save.el" *emacs-etc/* )
   ;; remove clocked tasks with 0:00 duration
   org-clock-out-remove-zero-time-clocks t
   org-clock-persist 'history
   ;; Resume when clocking into task with open clock
   org-clock-in-resume t)
  :config
  (add-hook 'kill-emacs-hook #'org-clock-save))



(use-package org-refile
  :straight nil
  :defer t
  :init
       ;;;; refile, todo
  (setq org-refile-targets
	'((org-agenda-files . (:maxlevel . 2))
	  (nil . (:maxlevel . 2))))
  (setq org-refile-use-outline-path t)
  (setq org-refile-allow-creating-parent-nodes 'confirm)
  (setq org-refile-use-cache t)
  (setq org-reverse-note-order nil)
  (setq org-outline-path-complete-in-steps nil)
  ;;	(setq org-refile-target-verify-function #'vulpea-refile-verify-target)
  )



(use-package org-archive
  :straight nil
  :defer t
  :init
  (setq-default
   org-archive-location
   (concat org-directory ".archive/%s_archive" "::" "datetree/*")
   org-archive-save-context-info
   '(time file ltags itags todo category olpath)))



(use-package org-id
  :straight nil
  :defer t
  :hook ( ;;(before-save . vulpea-id-auto-assign)
         (org-capture-prepare-finalize . org-id-get-create))
  :init
  (setq org-id-uuid-program
        "uuidgen | tr \"[:upper:]\" \"[:lower:]\"")
  :config
  ;;(org-link-set-parameters
  ;; "id" :activate-func #'vulpea-activate-link)
  (setq
   org-id-track-globally t
   org-id-extra-files
   (list (expand-file-name ".archive/archive" org-directory)
         (expand-file-name ".archive/archive.org" org-directory))
   org-id-link-to-org-use-id t
   org-id-locations-file (expand-file-name "org-id-locations" *xdg-cache/*)))



(use-package org-attach
  :straight nil
  :defer t
  :config
  (setq-default
   org-attach-id-dir (expand-file-name ".data/" org-directory)
   org-attach-auto-tag nil
   org-attach-file-list-property nil
   org-attach-store-link-p 'attached))

(provide 'org-agenda-defaults)
;;; org-agenda-defaults.el ends here
