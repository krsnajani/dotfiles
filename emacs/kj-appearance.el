
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
  (load-theme 'kj t))

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
		    :font "SFMono Nerd Font Mono"
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
