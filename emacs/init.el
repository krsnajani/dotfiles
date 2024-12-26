(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
 (package-install 'use-package))


;; Configs

(load-file "~/.config/emacs/kj-appearance.el") ;; Themes, fonts etc
(load-file "~/.config/emacs/kj-elfeed.el") ;; Themes, fonts etc
(load-file "~/.config/emacs/kj-denote.el") ;; The Denote Note Taking Package
(load-file "~/.config/emacs/kj-org.el") ;; Org mode and text mode changes
(load-file "~/.config/emacs/miniflux.el") ;; Mail changes

(server-start)
(require 'org-protocol)

(use-package pdf-tools
  :ensure t
  :defer t
  :commands (pdf-loader-install)
  :mode "\\.pdf\\'"
  :init (pdf-tools-install)
  :config (add-to-list 'revert-without-query ".pdf"))

(use-package consult
  :ensure t
  :bind
  ("C-x b" . 'consult-buffer)
  ("C-c s" . 'consult-line)
  ("C-c f" . 'consult-find))

(use-package vertico
  :ensure t
  :config
  (vertico-mode 1))

(use-package marginalia
  :ensure t
  :config
  (marginalia-mode +1))

(use-package magit
  :ensure t
  ) 

(use-package embark
  :ensure t
  :bind
  ("C-." . 'embark-act))

(setq-default line-spacing 0.15)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(browse-url-browser-function 'eww-browse-url)
 '(citar-bibliography '("~/docs/Library.bib"))
 '(ef-themes-to-toggle '(ef-autumn ef-day))
 '(elfeed-tube-mpv-options '("--cache=yes" "--force-window=yes" "--profile=720p"))
 '(evil-want-keybinding nil)
 '(make-backup-files nil)
 '(miniflux-server "http://umbrel:2520/v1")
 '(miniflux-token "GXx63OXAWWJx5K5WB1GcX-jxj2OnJCb8nVOXztz0g1Y=")
 '(org-agenda-files
   '("/home/krishnaj/docs/org/capture.org"
     "/home/krishnaj/docs/org/refile.org"
     "/home/krishnaj/docs/org/schedule.org"))
 '(org-cite-csl-styles-dir "/home/krishnaj/Zotero/styles")
 '(org-cite-global-bibliography '("~/docs/Library.bib"))
 '(package-selected-packages
   '(all-the-icons all-the-icons-nerd-fonts citeproc citeproc-org
		   consult-notes denote desktop-environment
		   doom-modeline doom-themes ef-themes elfeed-goodies
		   elfeed-org elfeed-tube embark embark-consult
		   jazz-theme magit marginalia markdown-mode
		   modus-themes olivetti org-caldav org-journal
		   org-modern org-roam org-static-blog pdf-tools
		   rainbow-mode subsonic sudo-edit vertico vterm
		   weblorg))
 '(select-enable-clipboard t)
 '(smtpmail-smtp-server "posteo.de")
 '(smtpmail-smtp-service 587)
 '(subsonic-ssl nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
