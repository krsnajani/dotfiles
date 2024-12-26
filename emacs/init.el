(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
 (package-install 'use-package))


;; Configs

;(load-file "~/.config/emacs/kj-appearance.el") ;; Themes, fonts etc
;(load-file "~/.config/emacs/kj-elfeed.el") ;; Themes, fonts etc
;(load-file "~/.config/emacs/kj-denote.el") ;; The Denote Note Taking Package
(load-file "~/.config/emacs/kj-org.el") ;; Org mode and text mode changes
;(load-file "~/.config/emacs/miniflux.el") ;; Mail changes

(server-start)
(require 'org-protocol)

(use-package vterm
  :ensure t
 )


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


;; Generic UI Changes. Disables all GUI elements, such as toolbar, scrollbar and menubar

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(recentf-mode)
(visual-line-mode +1)


;; Installing Themes

(use-package modus-themes
  :ensure t
  :config
  (load-theme 'jazz t))

;; Although modus themes are now default in Emacs, there are certain modus variants that are not part of the default package

(use-package ef-themes
  :ensure t
  :config
 ;; (load-theme 'acme t)
  (global-set-key (kbd "<f6>") 'modus-themes-toggle))
  

(use-package doom-themes
  :ensure t)

(use-package doom-modeline
  :ensure
  :config
  (doom-modeline-mode 1)
  (display-battery-mode 1))
;; The lighting suits the theme so allows for frequent changes


;; Simple font configuration
(set-face-attribute 'default nil
		    :font "SFMono Nerd Font Mono"
		    :height 120)
(set-face-attribute 'variable-pitch nil
		    :font "Liberation Sans"
		    :height 120)
(set-face-attribute 'fixed-pitch nil
		    :font "SFMono Nerd Font Mono"
		    :height 130)

;; Makes commented text and keywords italics.
(set-face-attribute 'font-lock-comment-face nil
		    :slant 'italic)
(set-face-attribute 'font-lock-keyword-face nil
		    :slant 'italic)

(defun kj/toggle-theme ()
  "Toggle the deeper-blue theme.

URL `http://xahlee.info/emacs/emacs/elisp_xah-toggle-theme.html`
Created: 2024-08-22
Version: 2024-08-29"
  (interactive)
  (if custom-enabled-themes
      (progn
        (disable-theme 'deeper-blue)
        (set-background-color "honeydew"))
    (load-theme 'deeper-blue)))



 (setq gc-cons-threshold most-positive-fixnum ; 2^61 bytes
       gc-cons-percentage 0.6)
 (defvar config:file-name-handler-alist-cache file-name-handler-alist)
 (setq file-name-handler-alist nil)
 (defun config:restore-post-init-settings ()
   (setq gc-cons-threshold 16777216 ; 16mb
         gc-cons-percentage 0.1)
   (setq file-name-handler-alist config:file-name-handler-alist-cache))
 (add-hook 'emacs-startup-hook #'config:restore-post-init-settings)
 
 (defun config:defer-gc ()
   (setq gc-cons-threshold most-positive-fixnum))
 (defun config:-do-restore-gc ()
   (setq gc-cons-threshold 16777216))
 (defun config:restore-gc ()
   (run-at-time 1 nil #'config:-do-restore-gc))
 
 (add-hook 'minibuffer-setup #'config:defer-gc)
 (add-hook 'minibuffer-exit #'config:restore-gc)

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
