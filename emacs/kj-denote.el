;; Note Taking and Denote Configuration

(use-package consult-notes
  :ensure t
  :config
  (consult-notes-denote-mode))

(use-package denote
  :ensure t
  :config
  (setq denote-directory (expand-file-name "~/docs/denote/"))
  (setq denote-known-keywords '("emacs" "philosophy" "politics" "economics" "booknotes"))
  (setq denote-sort-keywords t)
  (setq denote-file-type nil) ; Org is the default, set others here
  (setq denote-prompts '(title keywords))
  (setq denote-excluded-directories-regexp nil)
  (setq denote-excluded-keywords-regexp nil)
  (let ((map global-map))
    (define-key map (kbd "M-n n") #'consult-notes)
    (define-key map (kbd "M-n c") #'denote-region) ; "contents" mnemonic
    (define-key map (kbd "M-n N") #'denote-type)
    (define-key map (kbd "M-n d") #'denote-date)
    (define-key map (kbd "M-n z") #'denote-signature) ; "zettelkasten" mnemonic
    (define-key map (kbd "M-n s") #'denote-subdirectory)
    (define-key map (kbd "M-n t") #'denote-template)
    ;; If you intend to usM note with a variety of file types, it is
    ;; easier to bind the Minrelated commands to the `global-map', as
    ;; shown here.  OtherwMsnollow the same pattern for `org-mode-map',
    ;; `markdown-mode-map'M n/or `text-mode-map'.
    (define-key map (kbd "M-n i") #'denote-link) ; "insert" mnemonic
    (define-key map (kbd "M-n I") #'denote-add-links)
    (define-key map (kbd "M-n b") #'denote-backlinks)
    (define-key map (kbd "M-n f f") #'denote-find-link)
    (define-key map (kbd "M-n f b") #'denote-find-backlink)
    ;; Note that `denote-rMnn-file' can work from any context, not just
    ;; Dired bufffers.  ThMtn why we bind it here to the `global-map'.
    (define-key map (kbd "M-n r") #'denote-rename-file)
    (define-key map (kbd "M-n r") #'denote-rename-file-using-front-matter)
    (define-key map (kbd "M-n o") #'denote-open-or-create)))
