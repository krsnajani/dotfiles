
(use-package elfeed
  :ensure t
  :config
  (setq elfeed-feeds
	'(("https://planet.emacslife.com/atom.xml" emacs)
	  ("https://lobste.rs/rss" tech news)
	  ("https://feeds.npr.org/1001/rss.xml" news)
	  )))


(use-package elfeed-goodies
  :ensure t
  :after elfeed
  :config
  (elfeed-goodies/setup))

(use-package elfeed-tube
  :ensure t
  :after elfeed
  :config
  (elfeed-tube-setup)
  :bind
  (:map elfeed-show-mode-map
	("F" . elfeed-tube-fetch)
	([remap save-buffer] . elfeed-tube-save)
	:map elfeed-search-mode-map
	("F" . elfeed-tube-fetch)
	([remap save-buffer] . elfeed-tube-save)))
