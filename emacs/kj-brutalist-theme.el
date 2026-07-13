;;; kj-brutalist-theme.el --- A high-contrast industrial focus theme
;; Part of the KJ Focus Pack (Theme 4/7)

(deftheme kj-brutalist "A raw, greyscale industrial theme")

(let* ((bg-main           "#dcdcdc") ; Gainsboro (Concrete)
       (bg-alt            "#d3d3d3") ; LightGray
       (bg-hl             "#c0c0c0") ; Silver (Highlight)
       (bg-region         "#a9a9a9") ; DarkGray (Marked)
       (bg-block          "#e8e8e8") ; Lighter block
       
       ;; Text
       (fg-main           "#000000") ; Pure Black
       (fg-dim            "#696969") ; DimGray
       
       ;; Palette (Safety Colors)
       (color-black       "#000000") 
       (color-safety      "#ff4500") ; OrangeRed (Attention!)
       (color-go          "#32cd32") ; LimeGreen
       (color-link        "#444444") ; Dark grey link
       
       ;; UI
       (cursor-col        "#ff4500") ; High Vis Cursor
       (mode-active-bg    "#000000") ; Black
       (mode-active-fg    "#ffffff") ; White
       (mode-inactive-bg  "#bebebe")
       (mode-inactive-fg  "#000000"))

  (custom-theme-set-faces
   'kj-brutalist

   ;; --- Basics ---
   `(default ((t (:background ,bg-main :foreground ,fg-main))))
   `(cursor ((t (:background ,cursor-col))))
   `(region ((t (:background ,bg-region :foreground "#ffffff" :extend t))))
   `(highlight ((t (:background ,bg-hl :extend t))))
   `(hl-line ((t (:background ,bg-hl :extend t))))
   
   ;; Olivetti Fix
   `(fringe ((t (:background ,bg-main :foreground ,fg-dim))))
   `(vertical-border ((t (:foreground ,bg-alt :background ,bg-main))))
   `(window-divider ((t (:foreground ,bg-alt :background ,bg-main))))
   
   `(link ((t (:foreground ,color-link :underline t))))
   
   ;; --- Line Numbers ---
   `(line-number ((t (:foreground "#808080" :background ,bg-alt))))
   `(line-number-current-line ((t (:background ,bg-hl :foreground ,color-black :weight bold))))

   ;; --- Syntax (Brutalist = Bold vs Normal, few colors) ---
   `(font-lock-comment-face ((t (:foreground ,fg-dim :slant italic))))
   `(font-lock-keyword-face ((t (:foreground ,color-black :weight bold)))) 
   `(font-lock-string-face ((t (:foreground "#444444" :underline t)))) ; Underlined strings
   `(font-lock-constant-face ((t (:foreground ,color-black :weight bold))))
   `(font-lock-builtin-face ((t (:foreground ,color-black :weight bold))))
   `(font-lock-type-face ((t (:foreground ,color-black :weight bold :slant italic))))
   `(font-lock-function-name-face ((t (:foreground ,color-black :weight bold))))
   ;; VARIABLES get High Vis Green to stand out in the logic
   `(font-lock-variable-name-face ((t (:background "#ccffcc" :foreground ,color-black))))
   `(font-lock-warning-face ((t (:background ,color-safety :foreground "#ffffff" :weight bold))))
   `(font-lock-doc-face ((t (:foreground ,fg-dim))))

   ;; --- ORG MODE (The Manifesto) ---
   `(org-hide ((t (:foreground ,bg-main))))
   
   ;; Headings: ALL CAPS style sizing, pure black
   ;; Fixed: Changed weight 900 to 'ultra-bold
   `(org-level-1 ((t (:foreground "#000000" :weight ultra-bold :height 1.4 :background "#d3d3d3" :extend t))))
   `(org-level-2 ((t (:foreground "#000000" :weight bold :height 1.2 :underline t))))
   `(org-level-3 ((t (:foreground "#333333" :weight bold))))
   `(org-level-4 ((t (:foreground "#555555" :weight bold))))
   
   `(org-block ((t (:background ,bg-block :foreground ,fg-main :extend t))))
   `(org-block-begin-line ((t (:foreground ,fg-dim :background ,bg-block :slant italic))))
   `(org-block-end-line ((t (:foreground ,fg-dim :background ,bg-block :slant italic))))
   ;; Fixed: Changed weight 900 to 'ultra-bold
   `(org-document-title ((t (:foreground "#000000" :weight ultra-bold :height 1.6 :underline t))))
   
   ;; Todos (Industrial Labels)
   `(org-todo ((t (:foreground "#ffffff" :background "#000000" :weight bold :box (:line-width 1 :color "#000000")))))
   `(org-done ((t (:foreground "#000000" :weight bold :strike-through t :box (:line-width 1 :color "#000000")))))
   `(org-headline-done ((t (:foreground ,fg-dim :strike-through t))))

   ;; --- ORG AGENDA ---
   ;; Fixed: Changed weight 900 to 'ultra-bold
   `(org-agenda-date ((t (:foreground "#000000" :weight ultra-bold :height 1.1))))
   `(org-agenda-date-today ((t (:foreground "#000000" :weight ultra-bold :underline t :height 1.1))))
   `(org-agenda-structure ((t (:foreground "#000000" :weight bold :height 1.2 :underline t))))
   `(org-upcoming-deadline ((t (:background ,color-safety :foreground "#ffffff" :weight bold))))
   `(org-scheduled-today ((t (:foreground "#000000" :weight bold :underline t))))
   
   ;; --- TOOLS ---
   ;; Fixed: Changed weight 900 to 'ultra-bold
   `(dired-directory ((t (:foreground "#000000" :weight ultra-bold))))
   `(dired-symlink ((t (:foreground "#555555" :slant italic))))
   `(dired-marked ((t (:background "#000000" :foreground "#ffffff" :weight bold))))
   
   ;; Fixed: Changed weight 900 to 'ultra-bold
   `(magit-section-heading ((t (:foreground "#000000" :weight ultra-bold :underline t))))
   `(magit-diff-added ((t (:background "#ccffcc" :foreground "#000000"))))
   `(magit-diff-removed ((t (:background "#ffcccc" :foreground "#000000"))))
   `(magit-header-line ((t (:background ,mode-active-bg :foreground ,mode-active-fg))))
   
   ;; --- Mode Line ---
   `(mode-line ((t (:background ,mode-active-bg :foreground ,mode-active-fg
                    :box (:line-width 1 :color ,mode-active-bg :style nil)))))
   `(mode-line-inactive ((t (:background ,mode-inactive-bg :foreground ,mode-inactive-fg
                             :box (:line-width 1 :color ,mode-inactive-bg :style nil)))))
   
   `(isearch ((t (:background ,color-safety :foreground "#ffffff"))))
   ))

(add-hook 'prog-mode-hook 'hl-line-mode)
(setq-default cursor-type 'box)
(provide-theme 'kj-brutalist)
