;; Org Mode

(use-package org-modern
  :ensure t
  )

(defun my-org-faces ()
  (set-face-attribute 'org-todo nil :height 0.8)
  (set-face-attribute 'org-level-1 nil :height 1.3)
  (set-face-attribute 'org-level-2 nil :height 1.2)
  (set-face-attribute 'org-level-3 nil :height 1.1))

(add-hook 'text-mode-hook #'(lambda ()
			      (olivetti-mode)
			      (flyspell-mode)))

(add-hook 'org-mode-hook #'(lambda ()
			     (setq org-hide-emphasis-markers t)
			     (org-modern-mode)
			     (org-indent-mode)
			     (my-org-faces)))

(add-hook 'olivetti-mode-on-hook (lambda ()
				   (visual-line-mode)
				   (olivetti-set-width 90)))

(customize-set-variable 'ispell-program-name "aspell")
(customize-set-variable 'ispell-extra-args '("--sug-mode=ultra"))
