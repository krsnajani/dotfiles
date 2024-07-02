
;; Org Mode

(use-package org-modern
  :ensure t
  )

(defun my-org-faces ()
  (set-face-attribute 'org-todo nil :height 0.8)
  (set-face-attribute 'org-level-1 nil :height 1.3)
  (set-face-attribute 'org-level-2 nil :height 1.2)
  (set-face-attribute 'org-level-3 nil :height 1.1))

(add-hook 'text-mode-hook #'(lambda ()
			      (olivetti-mode)
			      (flyspell-mode)))

(add-hook 'org-mode-hook #'(lambda ()
			     (setq org-hide-emphasis-markers t)
			     (org-modern-mode)
			     (org-indent-mode)
			     (my-org-faces)
			     (variable-pitch-mode)))

(add-hook 'olivetti-mode-on-hook (lambda ()
				   (visual-line-mode)
				   (olivetti-set-width 120)))

(customize-set-variable 'ispell-program-name "aspell")
(customize-set-variable 'ispell-extra-args '("--sug-mode=ultra"))

(use-package org
  :ensure t
  :config

  (setq org-directory "~/docs/org")
  (global-set-key (kbd "C-c a") 'org-agenda)
  (global-set-key (kbd "C-c c") 'org-capture)
  (setq org-capture-templates
	'(("t" "Todo" entry (file+headline "~/docs/org/capture.org" "Tasks")
           "** TODO %?\n  %i\n  %a")
	  ("s" "Scheduled Time Blocks" entry (file+headline "~/docs/org/schedule.org" "Time Blocks")
	   "** Work On : %?\n SCHEDULED: %T\n")
	  ("e" "Event" entry (file+headline "~/docs/org/schedule.org" "Events")
	   "** %?\n SCHEDULED: %T\n %a")
	  ("a" "Assignment Entry" entry (file+headline "~/docs/org/memory.org" "Assignments")
	   "** %?\n Subject : %?\n DEADLINE:%T\n")
	  ("c" "Cool New Thing" entry (file "~/docs/org/archive.org"))
	  )))

(use-package org-caldav
  :ensure t
  :config
  (setq org-caldav-url "https://posteo.de:8443/calendars/krishnajani/")
  (setq org-caldav-calendar-id "default")
  (setq org-caldav-inbox "~/docs/org/schedule.org")
  (setq org-caldav-files '("~/docs/org/memory.org"))
  (setq org-icalendar-timezone "Asia/Kolkata")
  (setq org-icalendar-include-todo 'all
	org-caldav-sync-todo t
	org-icalendar-use-deadline '(event-if-todo event-if-not-todo todo-due)
	org-icalendar-use-scheduled '(event-if-todo event-if-not-todo todo-start)
	org-icalendar-with-timestamps t)
  )

;; Todo
;; Assignments
;; Time Blocks
;; 
