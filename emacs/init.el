;; -*- lexical-binding: t; -*-
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
 (package-install 'use-package))


;; Configs
(load-file "~/.emacs.d/kj-org.el") ;; Org mode and text mode changes

(setq ispell-dictionary "british")
(setq send-mail-function 'smtpmail-send-it)
(setq smtpmail-smtp-server "posteo.de")
(setq smtpmail-smtp-service 587)
(setq smtpmail-smtp-user "krishnajani@posteo.com")
(setq smtpmail-servers-requiring-authorization "posteo.de")
;(org-babel-load-file "scimax-editmarks.org")

;(server-start)
(require 'org-protocol)

(setq package-install-upgrade-built-in t)

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



(set-face-attribute 'mode-line nil :height 140) ;; Sets height to 14pt (140 tenths of a point)
(set-face-attribute 'mode-line-inactive nil :height 140)

(defun my-configure-font (frame)
  "Configure font given initial non-daemon FRAME.
Intended for `after-make-frame-functions'."
  (set-face-attribute 'default nil
		      :font "LiterationMono Nerd Font"
		      :height 140)
  (set-face-attribute 'variable-pitch nil
		      :font "Verdana"
		      :height 140)
  (set-face-attribute 'fixed-pitch nil
		      :font "PT Mono"
		      :height 140))
;  (remove-hook 'after-make-frame-functions #'my-configure-font))

(add-hook 'after-make-frame-functions #'my-configure-font)
;; Simple font configuration
(set-face-attribute 'default nil
		    :font "LiterationMono Nerd Font"
		    :height 130)
(set-face-attribute 'variable-pitch nil
		    :font "Verdana"
		    :height 140)
(set-face-attribute 'fixed-pitch nil
		    :font "PT Mono"
		    :height 140)
(global-set-key (kbd "C-c w") 'count-words)


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



(set-register ?s (cons 'file "~/Documents/scratchpad.org"))



(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1)
  (setq yas-snippet-dirs
	'("~/.emacs.d/snippets")))

(defun kj/sync-mail () (interactive)
       (async-shell-command "sh ~/.local/bin/mm"))

(defun kj/insert-date () (interactive)
       (insert (shell-command-to-string "echo -n $(date +%Y-%m-%d)")))


;(set-background-color "black")
;(set-foreground-color "white")


(load-theme 'modus-operandi 1)

    
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(citar-bibliography '("~/Documents/Library.bib"))
 '(custom-safe-themes
   '("850af01ee34a0d0a0b19ff7ef239bde103cbb87f31ce54d4cd6b17de45e632bb"
     default))
 '(evil-want-keybinding nil)
 '(inverse-video t)
 '(make-backup-files nil)
 '(org-agenda-files
   '("~/Library/Mobile Documents/com~apple~CloudDocs/Org/scratchpad.org"))
 '(org-cite-global-bibliography
   '("~/Library/Mobile Documents/com~apple~CloudDocs/Documents/Library.bib"))
 '(org-file-apps
   '((auto-mode . emacs) (directory . emacs) ("\\.mm\\'" . default)
     ("\\.x?html?\\'" . default) ("\\.pdf\\'" . system)))
 '(org-image-max-width 500)
 '(org-list-allow-alphabetical t)
 '(org-roam-capture-templates
   '(("d" "default" plain "%?" :target
      (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
		 "#+title: ${title} #+cite_export: csl /home/kjani/Zotero/styles/oscola.csl\12")
      :unnarrowed t)))
 '(package-selected-packages
   '(calfw-org consult magit marginalia olivetti org-modern org-roam
	       vertico yasnippet))
 '(select-enable-clipboard t))
    

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
