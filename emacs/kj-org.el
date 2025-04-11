
;; Org Mode

(use-package org-modern
  :ensure t
  :defer t
  )

(use-package olivetti
  :ensure t
  :defer t 
  )

(defun my-org-faces ()
  (set-face-attribute 'org-todo nil :height 0.8)
  (set-face-attribute 'org-level-1 nil :height 1.2)
  (set-face-attribute 'org-level-2 nil :height 1.1)
  (set-face-attribute 'org-level-3 nil :height 1.0))

(add-hook 'text-mode-hook #'(lambda ()
			      (olivetti-mode)))
;;			      (flyspell-mode)))

(add-hook 'org-mode-hook #'(lambda ()
			     (setq org-hide-emphasis-markers t)
			     (org-modern-mode)
			     (org-indent-mode)
			     ;(variable-pitch-mode)
			     (my-org-faces)))

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
  (setq org-agenda-directory "~/docs/org")
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
	  ("r" "Read It Later" entry (file "~/docs/org/read-later.org"))
	  ("L" "org-capture websites" entry (file "~/docs/org/archive.org")
	   "* %^{Title}\n\n  Source: %u, %c\n\n  %i")
	  ("p" "org-capture text from websites" entry (file "~/docs/org/archive.org")
	   "* %^{Title}\n\n  Source: %u, %c\n\n  %i")
	  )))

(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory "~/docs/org/roam")
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert))
  :config
  (org-roam-setup))

(setq org-latex-packages-alist '(("margin=2cm" "geometry" nil)))
