(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-install 'use-package))


;; Configs
(load-file "~/.config/emacs/kj-appearance.elc") ;; Themes, fonts etc
(load-file "~/.config/emacs/kj-denote.elc") ;; The Denote Note Taking Package
(load-file "~/.config/emacs/kj-org.elc") ;; Org mode and text mode changes
(load-file "~/.config/emacs/lisp/kj-rmail.elc") ;; Mail changes


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
