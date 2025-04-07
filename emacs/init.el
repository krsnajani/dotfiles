(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
 (package-install 'use-package))


;; Configs
(load-file "~/.config/emacs/kj-org.el") ;; Org mode and text mode changes


;(server-start)
(require 'org-protocol)


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
  :defer t
  :bind
  ("C-x g" . 'magit)
  ) 

(setq-default line-spacing 0.15)


;; Generic UI Changes. Disables all GUI elements, such as toolbar, scrollbar and menubar

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(recentf-mode)
(visual-line-mode -1)

(use-package spacious-padding
  :ensure t
  :config
  (spacious-padding-mode 1))

(load-theme 'modus-operandi 1)

(defun my-configure-font (frame)
  "Configure font given initial non-daemon FRAME.
Intended for `after-make-frame-functions'."
  (set-face-attribute 'default nil
		      :font "SFMono Nerd Font Mono"
		      :height 130)
  (set-face-attribute 'variable-pitch nil
		      :font "TrebuchetMS"
		      :height 140)
  (set-face-attribute 'fixed-pitch nil
		      :font "SFMono Nerd Font Mono"
		      :height 140))
;  (remove-hook 'after-make-frame-functions #'my-configure-font))

(add-hook 'after-make-frame-functions #'my-configure-font)
;; Simple font configuration
(set-face-attribute 'default nil
		    :font "SFMono Nerd Font Mono"
		    :height 140)
(set-face-attribute 'variable-pitch nil
		    :font "Liberation Sans"
		    :height 140)
(set-face-attribute 'fixed-pitch nil
		    :font "SFMono Nerd Font Mono"
		    :height 140)

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
 '(citar-bibliography '("~/docs/Library.bib"))
 '(evil-want-keybinding nil)
 '(make-backup-files nil)
 '(org-agenda-files '("~/docs/org/schedule.org"))
 '(org-cite-global-bibliography '("~/docs/Library.bib"))
 '(package-selected-packages
   '(org-drill deft acme-theme ewal-doom-themes spacious-padding nano-agenda nanowrimo nano-theme all-the-icons all-the-icons-nerd-fonts citeproc citeproc-org consult-notes denote desktop-environment doom-modeline doom-themes ef-themes elfeed-goodies elfeed-org elfeed-tube embark embark-consult hemisu-theme jazz-theme magit marginalia markdown-mode modus-themes obsidian olivetti org-caldav org-journal org-modern org-roam org-static-blog pdf-tools rainbow-mode subsonic sudo-edit treesit-auto vertico vterm weblorg))
 '(select-enable-clipboard t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
