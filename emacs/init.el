(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; Download Evil
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

;; Download Evil

(use-package doom-themes
  :ensure t
  :config
  (load-theme 'modus-vivendi t))

(global-set-key (kbd "<f6>") 'modus-themes-toggle)

(use-package vterm
  :ensure t
  :bind
  ("C-c t" . 'vterm))

(use-package pdf-tools
  :ensure t
  :defer t
  :commands (pdf-loader-install)
  :mode "\\.pdf\\'"
  :init (pdf-loader-install)
  :config (add-to-list 'revert-without-query ".pdf"))
  

(use-package elfeed
  :ensure t
  :config
  (setq elfeed-feeds
	'("http://nullprogram.com/feed/"
          "https://planet.emacslife.com/atom.xml"
	  "https://rss.sciencedirect.com/publication/science/01674048"
	  "https://onlinelibrary.wiley.com/feed/17459125/most-recent"
	  "https://ieeexplore.ieee.org/rss/TOC7886829.XML"))
  :bind
  ("C-x w" . 'elfeed))


(use-package consult
  :ensure t
  :bind
  ("C-x b" . 'consult-buffer)
  ("C-c s" . 'consult-line)
  ("C-c C-f" . 'consult-find))

(use-package embark
  :ensure t
  :bind
  ("C-." . 'embark-act)
  )

(use-package embark-consult
  :ensure t
  )

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

(use-package org-modern
  :ensure t
  )

(use-package denote
  :ensure t
  :config
  (setq denote-directory (expand-file-name "~/docs/denote/"))
  (setq denote-known-keywords '("emacs" "philosophy" "politics" "economics"))
  (setq denote-sort-keywords t)
  (setq denote-file-type nil) ; Org is the default, set others here
  (setq denote-prompts '(title keywords))
  (setq denote-excluded-directories-regexp nil)
  (setq denote-excluded-keywords-regexp nil)
  :bind
  ("C-c n" . 'denote))

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(recentf-mode)
(visual-line-mode +1)

(set-face-attribute 'default nil
 :font "IBM Plex Mono"
  :height 130)
(set-face-attribute 'variable-pitch nil
  :font "IBM Plex Serif"
  :height 130
  :weight 'medium)
(set-face-attribute 'fixed-pitch nil
  :font "IBM Plex Mono"
  :height 130)
;; Makes commented text and keywords italics.
;; This is working in emacsclient but not emacs.
;; Your font must have an italic face available.
(set-face-attribute 'font-lock-comment-face nil
  :slant 'italic)
(set-face-attribute 'font-lock-keyword-face nil
  :slant 'italic)

(add-hook 'org-mode-hook #'(lambda ()

                             ;; make the lines in the buffer wrap around the edges of the screen.
                             
                             ;; to press C-c q  or fill-paragraph ever again!
                             (visual-line-mode)
			     (org-modern-mode)
                             (org-indent-mode)))



;; This sets the default font on all graphical frames created after restarting Emacs.
;; Does the same thing as 'set-face-attribute default' above, but emacsclient fonts
;; Uncomment the following line if line spacing needs adjusting.
(setq-default line-spacing 0.12)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(make-backup-files nil)
 '(package-selected-packages
   '(all-the-icons auctex-latexmk auto-dictionary consult denote
		   doom-modeline doom-themes elfeed embark
		   embark-consult magit marginalia nyan-mode
		   org-modern pdf-tools rainbow-mode selectrum
		   spacious-padding standard-themes typing-game typit
		   vertico vterm zotxt)))
