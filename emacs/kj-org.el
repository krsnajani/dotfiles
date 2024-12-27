
;; Org Mode

(use-package org-modern
  :ensure t
  )

(use-package olivetti
  :ensure t
  )

(defun my-org-faces ()
  (set-face-attribute 'org-todo nil :height 0.8)
  (set-face-attribute 'org-level-1 nil :height 1.2)
  (set-face-attribute 'org-level-2 nil :height 1.1)
  (set-face-attribute 'org-level-3 nil :height 1.0))

(add-hook 'text-mode-hook #'(lambda ()
			      (olivetti-mode)
			      (flyspell-mode)))

(add-hook 'org-mode-hook #'(lambda ()
			     (setq org-hide-emphasis-markers t)
			     (org-modern-mode)
			     (org-indent-mode)
			     (variable-pitch-mode)
			     (my-org-faces)))

(add-hook 'olivetti-mode-on-hook (lambda ()
				   (visual-line-mode)
				   (olivetti-set-width 120)))

(customize-set-variable 'ispell-program-name "aspell")
(customize-set-variable 'ispell-extra-args '("--sug-mode=ultra"))

(use-package org-journal
  :ensure t
  :config
  (setq org-journal-dir "~/docs/org/journal"))


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
	  ("r" "Read It Later" entry (file "~/docs/org/read-later.org"))
	  ("L" "org-capture websites" entry (file "~/docs/org/archive.org")
	   "* %^{Title}\n\n  Source: %u, %c\n\n  %i")
	  ("p" "org-capture text from websites" entry (file "~/docs/org/archive.org")
	   "* %^{Title}\n\n  Source: %u, %c\n\n  %i")
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


(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory "~/docs/org/roam")
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert))
  :config
  (org-roam-setup))


(use-package org-static-blog
  :ensure t
  :config
  (setq org-static-blog-publish-title "Krishna's Scratchpad on the Internet")
  (setq org-static-blog-publish-url "https://krsnajani.github.io/")
  (setq org-static-blog-publish-directory "~/code/github-pages/")
  (setq org-static-blog-posts-directory "~/code/github-pages/posts/")
  (setq org-static-blog-drafts-directory "~/code/github-pages/drafts/")
  (setq org-static-blog-enable-tags t)
  (setq org-export-with-toc nil)
  (setq org-export-with-section-numbers nil)
;; Head
  (setq org-static-blog-page-header
	"<link rel='stylesheet' href='https://cdn.simplecss.org/simple.css'>")
  
  ;; This preamble is inserted at the beginning of the <body> of every page:
  ;;   This particular HTML creates a <div> with a simple linked headline
  (setq org-static-blog-page-preamble
	"<div class=\"header\">
  <a href=\"https://blog.krishnaj.space\">Home!</a> <a href=\"./aboutme.html\">About Me</a>
</div>")

  ;; This postamble is inserted at the end of the <body> of every page:
  ;;   This particular HTML creates a <div> with a link to the archive page
  ;;   and a licensing stub.
  (setq org-static-blog-page-postamble
"<center><a rel=\"license\" href=\"https://creativecommons.org/licenses/by-sa/3.0/\"><img alt=\"Creative Commons License\" style=\"border-width:0\" src=\"https://i.creativecommons.org/l/by-sa/3.0/88x31.png\" /></a><br /><span xmlns:dct=\"https://purl.org/dc/terms/\" href=\"https://purl.org/dc/dcmitype/Text\" property=\"dct:title\" rel=\"dct:type\">kjani.xyz</span> by <a xmlns:cc=\"https://creativecommons.org/ns#\" href=\"https://krsnajani.github.io\" property=\"cc:attributionName\" rel=\"cc:attributionURL\">Krishna Jani</a> is licensed under a <a rel=\"license\" href=\"https://creativecommons.org/licenses/by-sa/3.0/\">Creative Commons Attribution-ShareAlike 3.0 Unported License</a>.</center>")

  ;; This HTML code is inserted into the index page between the preamble and
  ;;   the blog posts
  (setq org-static-blog-index-front-matter
	"<h1> Krishna's Scratchpad on the Internet </h1>\n"))
