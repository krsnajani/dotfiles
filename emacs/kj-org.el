;; -*- lexical-binding: t; -*-

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
			      (olivetti-mode)
			      (flyspell-mode)
			      (visual-line-mode)
			      (variable-pitch-mode)))

(add-hook 'org-mode-hook #'(lambda ()
			     (setq org-hide-emphasis-markers t)
			     (org-modern-mode)
			     (org-indent-mode)
					;(variable-pitch-mode)
			     (my-org-faces)))

(add-to-list 'auto-mode-alist '("\\.typ\\'" . text-mode))


(add-hook 'olivetti-mode-on-hook (lambda ()
				   (visual-line-mode)
				   (olivetti-set-width 120)))

(customize-set-variable 'ispell-program-name "aspell")
(customize-set-variable 'ispell-extra-args '("--sug-mode=ultra"))

(setq org-export-with-smart-quotes t)


(use-package org
  :ensure t
  :config

  (setq org-directory "/Users/krishnajani/Library/Mobile Documents/com~apple~CloudDocs/Org/")
  (global-set-key (kbd "C-c a") 'org-agenda)
  (global-set-key (kbd "C-c c") 'org-capture)
  (setq org-agenda-directory "~/Org/")
  (setq org-capture-templates
	'(("t" "Todo" entry (file+headline "/Users/krishnajani/Library/Mobile Documents/com~apple~CloudDocs/Org/scratchpad.org" "Tasks")
           "** TODO %?\n  %i\n  %a")
	  ("s" "Scheduled Time Blocks" entry (file+headline "/Users/krishnajani/Library/Mobile Documents/com~apple~CloudDocs/Org/scratchpad.org" "Time Blocks")
	   "** Work On : %?\n SCHEDULED: %T\n")
	  ("e" "Event" entry (file+headline "/Users/krishnajani/Library/Mobile Documents/com~apple~CloudDocs/Org/scratchpad.org" "Events")
	   "** %?\n SCHEDULED: %T\n %a")
	  ("a" "Assignment Entry" entry (file+headline "/Users/krishnajani/Library/Mobile Documents/com~apple~CloudDocs/Org/scratchpad.org" "Assignments")
	   "** %?\n Subject : %?\n DEADLINE:%T\n")
	  ("c" "Capture" entry (file "/Users/krishnajani/Library/Mobile Documents/com~apple~CloudDocs/Org/scratchpad.org"))
	  )))

(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory "~/Documents/notes")
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert))
  :config
  (org-roam-setup))

(setq tex-run-command "xelatex")

(setq org-latex-compiler "xelatex")

(setq org-latex-packages-alist '(("margin=2cm" "geometry" nil)
				 ("" "fontspec")))

(global-set-key (kbd "C-x c") 'org-cite-insert)

(use-package calfw
  :ensure t
  :config
  (require 'calfw)
  )

(use-package calfw-org
  :ensure t
  :config
  (global-set-key (kbd "C-c A") 'calfw-org-open-calendar)
  )

(defun kj/office-export ()
  "Export Org file to ODT with a user-selected template using nice names."
  (interactive)
  ;; Ask if we should keep line breaks
  (let* ((preserve-line-breaks (y-or-n-p "Do you want to preserve line breaks? "))
         ;; Ask if we should export just the body (no header, footer, title, or author)
         (body-only (y-or-n-p "Body only?"))
         (templates '(("regular" . "/Users/krishnajani/Library/Mobile Documents/com~apple~CloudDocs/Org/ott/regular.ott")
                      ("abstract" . "/Users/krishnajani/Library/Mobile Documents/com~apple~CloudDocs/Org/ott/abstract-2.ott")
                        ("Template 3" . "~/path/to/template3.ott")))
         (nice-name (completing-read "Choose ODT template: " (mapcar 'car templates)))
         (template-path (cdr (assoc nice-name templates))))

    ;; Use `let` to locally bind export options
    (let ((org-export-preserve-breaks preserve-line-breaks)
          (org-export-with-toc (not body-only))
          (org-export-with-title (not body-only))
          (org-export-with-author (not body-only))
          (org-odt-styles-file template-path))

      ;; Export the current Org buffer to ODT
      (org-odt-export-to-odt))))


(use-package obsidian
  :ensure t
  :config
  (global-obsidian-mode t)
  (obsidian-backlinks-mode t)
  :custom
  ;; location of obsidian vault
  (obsidian-directory "~/LR")
  ;; Default location for new notes from `obsidian-capture'
  (obsidian-inbox-directory "Inbox")
  (obsidian-templates-directory "templates")
  (obsidian-daily-notes-directory "Daily")
  ;; Useful if you're going to be using wiki links
  (markdown-enable-wiki-links t)

  ;; These bindings are only suggestions; it's okay to use other bindings
  :bind (:map obsidian-mode-map
              ;; Create note
              ("C-c C-n" . obsidian-capture)
              ;; If you prefer you can use `obsidian-insert-wikilink'
              ("C-c C-l" . obsidian-insert-link)
              ;; Open file pointed to by link at point
              ("C-c C-o" . obsidian-follow-link-at-point)
              ;; Open a different note from vault
              ("C-c C-p" . obsidian-jump)
              ;; Follow a backlink for the current file
              ("C-c C-b" . obsidian-backlink-jump)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
