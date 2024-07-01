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
(load-file "~/.config/emacs/lisp/kj-rmail.el") ;; Mail changes



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
  ("C-c f" . 'consult-find))

(fido-mode)
(fido-vertical-mode)


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
 '(ef-themes-to-toggle '(ef-autumn ef-day))
 '(elfeed-feeds
   '("https://www.youtube.com/feeds/videos.xml?channel_id=UC2eYFnH61tmytImy1mTYvhA"
     "https://www.youtube.com/feeds/videos.xml?channel_id=UC7YOGHUfC1Tb6E4pudI9STA"
     "https://www.youtube.com/feeds/videos.xml?channel_id=UCld68syR8Wi-GY_n4CaoJGA"
     "https://www.youtube.com/feeds/videos.xml?channel_id=UCUyeluBRhGPCW4rPe_UvBZQ"
     "https://www.youtube.com/feeds/videos.xml?channel_id=UCVYamHliCI9rw1tHR1xbkfw"
     "https://www.youtube.com/feeds/videos.xml?channel_id=UCshObcm-nLhbu8MY50EZ5Ng"
     "https://www.youtube.com/feeds/videos.xml?channel_id=UCAiiOTio8Yu69c3XnR7nQBQ"
     "https://www.youtube.com/feeds/videos.xml?channel_id=UCxpeu8gvV77Z1wUrTpu5BUQ"
     "https://www.youtube.com/feeds/videos.xml?channel_id=UC0uTPqBCFIpZxlz_Lv1tk_g"
     "https://www.youtube.com/feeds/videos.xml?channel_id=UCJetJ7nDNLlEzDLXv7KIo0w"
     "https://www.youtube.com/feeds/videos.xml?channel_id=UCVls1GmFKf6WlTraIb_IaJg"
     "https://www.youtube.com/feeds/videos.xml?channel_id=UCXuqSBlHAE6Xw-yeJA0Tunw"
     "https://www.youtube.com/feeds/videos.xml?channel_id=UCafEZMU5s8geb9oIly6xTrg"
     "https://www.youtube.com/feeds/videos.xml?channel_id=UCQjBsscIa_mgEnSvWpm_9vw"
     "https://planet.emacslife.com/atom.xml" "https://lobste.rs/rss"
     "https://feeds.npr.org/1001/rss.xml"))
 '(elfeed-tube-mpv-options '("--cache=yes" "--force-window=yes" "--profile=720p"))
 '(evil-want-keybinding nil)
 '(make-backup-files nil)
 '(org-agenda-files
   '("/home/krishnaj/docs/org/capture.org"
     "/home/krishnaj/docs/org/refile.org"
     "/home/krishnaj/docs/org/schedule.org"))
 '(org-cite-csl-styles-dir "/home/krishnaj/Zotero/styles")
 '(org-cite-global-bibliography '("~/docs/Library.bib"))
 '(package-selected-packages
   '(acme-theme all-the-icons auctex-latexmk auto-dictionary citar
		citar-denote citeproc-org consult consult-notes
		dashboard dashboard-hackernews denote doom-modeline
		doom-themes ef-themes elfeed elfeed-goodies elfeed-org
		elfeed-tube elfeed-tube-mpv embark embark-consult
		ement evil-collection evil-leader ewal-doom-themes
		ewal-evil-cursors hackernews listen magit marginalia
		markdown-mode modus-themes mw-thesaurus nyan-mode
		oauth2-auto obsidian olivetti orderless org org-caldav
		org-modern pdf-tools plan9-theme rainbow-mode
		selectrum solo-jazz-theme spacious-padding speed-type
		standard-themes sudo-edit tmr treemacs
		twilight-bright-theme typing-game typit vertico
		visual-fill visual-fill-column vterm weblorg zotxt)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
