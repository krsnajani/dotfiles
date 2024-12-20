
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
  (load-theme 'modus-vivendi-tinted t))

;; Although modus themes are now default in Emacs, there are certain modus variants that are not part of the default package

(use-package ef-themes
  :ensure t
  :config
 ;; (load-theme 'acme t)
  (global-set-key (kbd "<f6>") 'ef-themes-toggle))
  

;; The lighting suits the theme so allows for frequent changes


;; Simple font configuration
(set-face-attribute 'default nil
		    :font "SFMono Nerd Font Mono"
		    :height 120)
(set-face-attribute 'variable-pitch nil
		    :font "SFMono Nerd Font Mono"
		    :height 150)
(set-face-attribute 'fixed-pitch nil
		    :font "SFMono Nerd Font Mono"
		    :height 130)

;; Makes commented text and keywords italics.
(set-face-attribute 'font-lock-comment-face nil
		    :slant 'italic)
(set-face-attribute 'font-lock-keyword-face nil
		    :slant 'italic)

