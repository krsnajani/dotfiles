;;; kj-blueprint-theme.el --- A structural engineering focus theme
;; Part of the KJ Focus Pack (Theme 2/7)

(deftheme kj-blueprint "A monochromatic structural blue theme")

(let* ((bg-main           "#bcd2ee") ; LightSteelBlue2
       (bg-alt            "#b0c4de") ; LightSteelBlue (slightly darker)
       (bg-hl             "#a2b5cd") ; LightSteelBlue3
       (bg-region         "#6495ed") ; CornflowerBlue (High contrast selection)
       (bg-block          "#b2cce5") ; Code block tint
       
       ;; Text
       (fg-main           "#191970") ; MidnightBlue
       (fg-dim            "#4a708b") ; SkyBlue4
       
       ;; Palette (The Blueprint)
       (color-structural  "#00008b") ; DarkBlue (Keywords/Structure)
       (color-data        "#000000") ; Black (Variables/Data)
       (color-meta        "#2f4f4f") ; DarkSlateGray (Metadata/Strings)
       (color-action      "#0000ff") ; Blue (Links/Functions)
       (color-alert       "#b22222") ; Firebrick (Errors - Rare warm tone)
       
       ;; Magit Specific (Blue-shifted diffs)
       (diff-add          "#c1d9ff")
       (diff-rem          "#d9c1c1")
       (diff-hunk         "#aabace")
       
       ;; UI
       (cursor-col        "#00008b")
       (mode-active-bg    "#191970") 
       (mode-active-fg    "#bcd2ee")
       (mode-inactive-bg  "#7a8b8b")
       (mode-inactive-fg  "#191970"))

  (custom-theme-set-faces
   'kj-blueprint

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
   
   `(link ((t (:foreground ,color-action :underline t))))
   
   ;; --- Line Numbers ---
   `(line-number ((t (:foreground "#708090" :background ,bg-alt))))
   `(line-number-current-line ((t (:background ,bg-hl :foreground ,color-structural :weight bold))))

   ;; --- Syntax (Structural Focus) ---
   `(font-lock-comment-face ((t (:foreground ,fg-dim :slant italic))))
   `(font-lock-keyword-face ((t (:foreground ,color-structural :weight bold)))) 
   `(font-lock-string-face ((t (:foreground ,color-meta))))
   `(font-lock-constant-face ((t (:foreground ,color-structural :weight bold))))
   `(font-lock-builtin-face ((t (:foreground ,color-structural))))
   `(font-lock-type-face ((t (:foreground ,color-structural :weight bold))))
   `(font-lock-function-name-face ((t (:foreground ,color-action :weight bold))))
   `(font-lock-variable-name-face ((t (:foreground ,color-data :weight bold))))
   `(font-lock-warning-face ((t (:foreground ,color-alert :weight bold))))
   `(font-lock-doc-face ((t (:foreground ,fg-dim))))

   ;; --- ORG MODE (The Blueprint) ---
   `(org-hide ((t (:foreground ,bg-main))))
   
   ;; Headings: Distinguished by Weight and Shade of Blue
   `(org-level-1 ((t (:foreground "#00008b" :weight bold :height 1.3))))
   `(org-level-2 ((t (:foreground "#104e8b" :weight bold :height 1.15)))) ; DodgerBlue4
   `(org-level-3 ((t (:foreground "#27408b" :weight bold)))) ; RoyalBlue4
   `(org-level-4 ((t (:foreground "#4a708b" :weight bold))))
   
   `(org-block ((t (:background ,bg-block :foreground ,fg-main :extend t))))
   `(org-block-begin-line ((t (:foreground ,fg-dim :background ,bg-block :slant italic))))
   `(org-block-end-line ((t (:foreground ,fg-dim :background ,bg-block :slant italic))))
   `(org-document-title ((t (:foreground ,color-structural :weight bold :height 1.5))))
   
   ;; Todos (High Contrast Blueprint style)
   `(org-todo ((t (:foreground "#ffffff" :background ,color-structural :weight bold :box (:line-width 1 :style nil)))))
   `(org-done ((t (:foreground ,fg-dim :weight bold :strike-through t))))
   `(org-headline-done ((t (:foreground ,fg-dim :strike-through t))))

   ;; --- ORG AGENDA ---
   `(org-agenda-date ((t (:foreground ,color-structural :weight bold :height 1.1))))
   `(org-agenda-date-today ((t (:foreground ,color-structural :weight bold :underline t :height 1.1))))
   `(org-agenda-structure ((t (:foreground ,color-data :weight bold :height 1.2))))
   `(org-upcoming-deadline ((t (:foreground ,color-alert :weight bold))))
   `(org-scheduled-today ((t (:foreground ,color-structural :weight bold))))
   
   ;; --- TOOLS ---
   `(dired-directory ((t (:foreground ,color-structural :weight bold))))
   `(dired-symlink ((t (:foreground ,color-action :slant italic))))
   `(dired-marked ((t (:background ,bg-region :foreground "#ffffff" :weight bold))))
   
   `(magit-section-heading ((t (:foreground ,color-structural :weight bold))))
   `(magit-diff-added ((t (:background ,diff-add :foreground "#005500"))))
   `(magit-diff-removed ((t (:background ,diff-rem :foreground "#8b0000"))))
   `(magit-header-line ((t (:background ,mode-active-bg :foreground ,mode-active-fg))))
   
   ;; --- Mode Line ---
   `(mode-line ((t (:background ,mode-active-bg :foreground ,mode-active-fg
                    :box (:line-width 1 :color ,mode-active-bg :style nil)))))
   `(mode-line-inactive ((t (:background ,mode-inactive-bg :foreground ,mode-inactive-fg
                             :box (:line-width 1 :color ,mode-inactive-bg :style nil)))))
   
   `(isearch ((t (:background "#ffff00" :foreground "#000000"))))
   ))

(add-hook 'prog-mode-hook 'hl-line-mode)
(setq-default cursor-type 'box)
(provide-theme 'kj-blueprint)
