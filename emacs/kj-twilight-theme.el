;;; kj-twilight-theme.el --- A hypnagogic purple focus theme  -*- lexical-binding: t; -*-
;; Part of the KJ Focus Pack (Theme 7/7)

(deftheme kj-twilight "A creative purple twilight theme")

(let* ((bg-main           "#dcd0ff") ; PalePurple (Custom/Approx)
       (bg-alt            "#d1c4e9") ; Slightly darker purple
       (bg-hl             "#e6e6fa") ; Lavender
       (bg-region         "#b39ddb") ; Deep Lavender (Selection)
       (bg-block          "#d4c6e8") ; Block bg
       
       ;; Text
       (fg-main           "#300060") ; Deep Indigo
       (fg-dim            "#6a5acd") ; SlateBlue (Comments)
       
       ;; Palette (The Dream)
       (color-deep        "#4b0082") ; Indigo (Keywords)
       (color-vivid       "#8a2be2") ; BlueViolet (Variables)
       (color-soft        "#551a8b") ; Purple4 (Strings)
       (color-royal       "#483d8b") ; DarkSlateBlue (Functions)
       (color-alert       "#8b008b") ; DarkMagenta (Warning)
       
       ;; Magit (Purple shift)
       (diff-add          "#d0e8ff") ; Cool blue-ish add
       (diff-rem          "#e8d0d0") ; Reddish remove
       
       ;; UI
       (cursor-col        "#4b0082")
       (mode-active-bg    "#4b0082") 
       (mode-active-fg    "#dcd0ff")
       (mode-inactive-bg  "#9575cd")
       (mode-inactive-fg  "#300060"))

  (custom-theme-set-faces
   'kj-twilight

   ;; --- Basics ---
   `(default ((t (:background ,bg-main :foreground ,fg-main))))
   `(cursor ((t (:background ,cursor-col))))
   `(region ((t (:background ,bg-region :extend t))))
   `(highlight ((t (:background ,bg-hl :extend t))))
   `(hl-line ((t (:background ,bg-hl :extend t))))
   
   ;; Olivetti Fix
   `(fringe ((t (:background ,bg-main :foreground ,fg-dim))))
   `(vertical-border ((t (:foreground ,bg-alt :background ,bg-main))))
   `(window-divider ((t (:foreground ,bg-alt :background ,bg-main))))
   
   `(link ((t (:foreground ,color-vivid :underline t))))
   
   ;; --- Line Numbers ---
   `(line-number ((t (:foreground "#9370db" :background ,bg-alt))))
   `(line-number-current-line ((t (:background ,bg-hl :foreground ,color-deep :weight bold))))

   ;; --- Syntax (Creative Focus) ---
   `(font-lock-comment-face ((t (:foreground ,fg-dim :slant italic))))
   `(font-lock-keyword-face ((t (:foreground ,color-deep :weight bold)))) 
   `(font-lock-string-face ((t (:foreground ,color-soft))))
   `(font-lock-constant-face ((t (:foreground ,color-royal :weight bold))))
   `(font-lock-builtin-face ((t (:foreground ,color-deep))))
   `(font-lock-type-face ((t (:foreground ,color-royal :weight bold))))
   `(font-lock-function-name-face ((t (:foreground ,color-vivid :weight bold))))
   `(font-lock-variable-name-face ((t (:foreground ,color-deep :weight bold))))
   `(font-lock-warning-face ((t (:foreground ,color-alert :weight bold))))
   `(font-lock-doc-face ((t (:foreground ,fg-dim))))

   ;; --- ORG MODE (The Lucid Dream) ---
   `(org-hide ((t (:foreground ,bg-main))))
   
   ;; Headings: Violet spectrum
   `(org-level-1 ((t (:foreground "#551a8b" :weight bold :height 1.3)))) ; Purple4
   `(org-level-2 ((t (:foreground "#8a2be2" :weight bold :height 1.15)))) ; BlueViolet
   `(org-level-3 ((t (:foreground "#9400d3" :weight bold)))) ; DarkViolet
   `(org-level-4 ((t (:foreground "#9932cc" :weight bold))))
   
   `(org-block ((t (:background ,bg-block :foreground ,fg-main :extend t))))
   `(org-block-begin-line ((t (:foreground ,fg-dim :background ,bg-block :slant italic))))
   `(org-block-end-line ((t (:foreground ,fg-dim :background ,bg-block :slant italic))))
   `(org-document-title ((t (:foreground ,color-deep :weight bold :height 1.5))))
   
   ;; Todos
   `(org-todo ((t (:foreground "#dcd0ff" :background "#551a8b" :weight bold :box (:line-width 1 :style nil)))))
   `(org-done ((t (:foreground ,fg-dim :weight bold :strike-through t))))
   `(org-headline-done ((t (:foreground ,fg-dim :strike-through t))))

   ;; --- ORG AGENDA ---
   `(org-agenda-date ((t (:foreground ,color-deep :weight bold :height 1.1))))
   `(org-agenda-date-today ((t (:foreground ,color-deep :weight bold :underline t :height 1.1))))
   `(org-agenda-structure ((t (:foreground ,color-vivid :weight bold :height 1.2))))
   `(org-upcoming-deadline ((t (:foreground ,color-alert :weight bold))))
   `(org-scheduled-today ((t (:foreground ,color-deep :weight bold))))
   
   ;; --- TOOLS ---
   `(dired-directory ((t (:foreground ,color-deep :weight bold))))
   `(dired-symlink ((t (:foreground ,color-vivid :slant italic))))
   `(dired-marked ((t (:background ,bg-region :foreground ,fg-main :weight bold))))
   
   `(magit-section-heading ((t (:foreground ,color-deep :weight bold))))
   `(magit-diff-added ((t (:background ,diff-add :foreground "#00008b"))))
   `(magit-diff-removed ((t (:background ,diff-rem :foreground "#8b008b"))))
   `(magit-header-line ((t (:background ,mode-active-bg :foreground ,mode-active-fg))))
   
   ;; --- Mode Line ---
   `(mode-line ((t (:background ,mode-active-bg :foreground ,mode-active-fg
                    :box (:line-width 1 :color ,mode-active-bg :style nil)))))
   `(mode-line-inactive ((t (:background ,mode-inactive-bg :foreground ,mode-inactive-fg
                             :box (:line-width 1 :color ,mode-inactive-bg :style nil)))))
   
   `(isearch ((t (:background "#9400d3" :foreground "#ffffff"))))
   ))

(add-hook 'prog-mode-hook 'hl-line-mode)
(setq-default cursor-type 'box)
(provide-theme 'kj-twilight)
