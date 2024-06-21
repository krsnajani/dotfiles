
;; Generic UI Changes. Disables all GUI elements, such as toolbar, scrollbar and menubar

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(recentf-mode)
(visual-line-mode +1)


;; Installing Thems

(use-package doom-themes
  :ensure t
  :config
  (load-theme 'modus-vivendi t))

;; Although modus themes are now default in Emacs, there are certain modus variants that are not part of the default package

(use-package modus-themes
  :ensure t
  )

;; The lighting suits the theme so allows for frequent changes
(global-set-key (kbd "<f6>") 'modus-themes-toggle)


;; Simple font configuration
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
(set-face-attribute 'font-lock-comment-face nil
		    :slant 'italic)
(set-face-attribute 'font-lock-keyword-face nil
		    :slant 'italic)

