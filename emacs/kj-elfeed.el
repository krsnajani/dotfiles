
(use-package elfeed
  :ensure t
 )

(use-package elfeed-goodies
  :ensure t
  :after elfeed
 )
;  (elfeed-goodies/setup))

(use-package elfeed-org
  :ensure t
  :after elfeed
  :config
  (setq rmh-elfeed-org-files (list "~/docs/org/elfeed.org"))
  (elfeed-org))

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
