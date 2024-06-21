(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-zenburn t))

(use-package modus-themes
  :ensure t
  )

(global-set-key (kbd "<f6>") 'modus-themes-toggle)

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(recentf-mode)
(visual-line-mode +1)

(set-face-attribute 'default nil
		    :font "Inconsolata Nerd Font"
		    :height 140)
(set-face-attribute 'variable-pitch nil
		    :font "Arial"
		    :height 140)
(set-face-attribute 'fixed-pitch nil
		    :font "Consolas"
		    :height 140)
;; Makes commented text and keywords italics.
;; This is working in emacsclient but not emacs.
;; Your font must have an italic face available.
(set-face-attribute 'font-lock-comment-face nil
		    :slant 'italic)
(set-face-attribute 'font-lock-keyword-face nil
		    :slant 'italic)

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
			     (my-org-faces)))

(add-hook 'olivetti-mode-on-hook (lambda ()
				   (visual-line-mode)
				   (olivetti-set-width 90)))

(customize-set-variable 'ispell-program-name "aspell")
(customize-set-variable 'ispell-extra-args '("--sug-mode=ultra"))

;; Denote - Note Taking

(use-package consult-notes
  :ensure t
  :config
  (consult-notes-denote-mode))

(use-package denote
  :ensure t
  :config
  (setq denote-directory (expand-file-name "~/docs/denote/"))
  (setq denote-known-keywords '("emacs" "philosophy" "politics" "economics" "booknotes"))
  (setq denote-sort-keywords t)
  (setq denote-file-type nil) ; Org is the default, set others here
  (setq denote-prompts '(title keywords))
  (setq denote-excluded-directories-regexp nil)
  (setq denote-excluded-keywords-regexp nil)
  (let ((map global-map))
    (define-key map (kbd "M-n n") #'consult-notes)
    (define-key map (kbd "M-n c") #'denote-region) ; "contents" mnemonic
    (define-key map (kbd "M-n N") #'denote-type)
    (define-key map (kbd "M-n d") #'denote-date)
    (define-key map (kbd "M-n z") #'denote-signature) ; "zettelkasten" mnemonic
    (define-key map (kbd "M-n s") #'denote-subdirectory)
    (define-key map (kbd "M-n t") #'denote-template)
    ;; If you intend to usM note with a variety of file types, it is
    ;; easier to bind the Minrelated commands to the `global-map', as
    ;; shown here.  OtherwMsnollow the same pattern for `org-mode-map',
    ;; `markdown-mode-map'M n/or `text-mode-map'.
    (define-key map (kbd "M-n i") #'denote-link) ; "insert" mnemonic
    (define-key map (kbd "M-n I") #'denote-add-links)
    (define-key map (kbd "M-n b") #'denote-backlinks)
    (define-key map (kbd "M-n f f") #'denote-find-link)
    (define-key map (kbd "M-n f b") #'denote-find-backlink)
    ;; Note that `denote-rMnn-file' can work from any context, not just
    ;; Dired bufffers.  ThMtn why we bind it here to the `global-map'.
    (define-key map (kbd "M-n r") #'denote-rename-file)
    (define-key map (kbd "M-n r") #'denote-rename-file-using-front-matter)
    (define-key map (kbd "M-n o") #'denote-open-or-create)))

;; Other Utils 

(use-package pdf-tools
  :ensure t
  :defer t
  :commands (pdf-loader-install)
  :mode "\\.pdf\\'"
  :init (pdf-loader-install)
  :config (add-to-list 'revert-without-query ".pdf"))

(use-package consult
  :ensure t
  :bind
  ("C-x b" . 'consult-buffer)
  ("C-c s" . 'consult-line)
  ("C-c C-f" . 'consult-find))

(use-package vertico
  :ensure t
  :config
  (vertico-mode +1))

(use-package marginalia
  :ensure t
  :config
  (marginalia-mode +1))

(use-package magit
  :ensure t
  ) 


;; This sets the default font on all graphical frames created after restarting Emacs.
;; Does the same thing as 'set-face-attribute default' above, but emacsclient fonts
;; Uncomment the following line if line spacing needs adjusting.
(setq-default line-spacing 0.15)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(citar-bibliography '("~/docs/Library.bib"))
 '(evil-want-keybinding nil)
 '(make-backup-files nil)
 '(org-agenda-files
   '("/home/krishna/git/notes/Working Memory.org" "/home/krishna/git/notes/Agenda.org"))
 '(org-cite-csl-styles-dir "/home/krishnaj/Zotero/styles")
 '(org-cite-global-bibliography '("~/docs/Library.bib"))
 '(package-selected-packages
   '(modus-themes evil-leader treemacs speed-type evil-collection hackernews dashboard-hackernews dashboard citeproc-org citar consult-notes citar-denote obsidian ewal-evil-cursors ewal-doom-themes sudo-edit olivetti markdown-mode all-the-icons auctex-latexmk auto-dictionary consult denote doom-modeline doom-themes elfeed embark embark-consult magit marginalia nyan-mode org-modern pdf-tools rainbow-mode selectrum spacious-padding standard-themes tmr typing-game typit vertico vterm weblorg zotxt)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
