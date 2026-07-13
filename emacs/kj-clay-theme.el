;;; kj-clay-theme.el --- An organic, tactile focus theme
;; Part of the KJ Focus Pack (Theme 3/7)

(deftheme kj-clay "A tactile earth-tone theme")

(let* ((bg-main           "#eecbad") ; PeachPuff2
       (bg-alt            "#deb887") ; Burlywood
       (bg-hl             "#ffdab9") ; PeachPuff1
       (bg-region         "#cdb79e") ; Bisque3
       (bg-block          "#e6c2a0") ; Slightly Darker Peach
       
       ;; Text
       (fg-main           "#3b2f2f") ; Very Dark Brown
       (fg-dim            "#8b7355") ; Burlywood4 (Comments)
       
       ;; Palette (The Earth)
       (color-clay        "#8b4513") ; SaddleBrown (Keywords)
       (color-stone       "#556b2f") ; DarkOliveGreen (Strings)
       (color-brick       "#8b2323") ; Brown4 (Variables)
       (color-sand        "#a0522d") ; Sienna (Types)
       (color-alert       "#cd3333") ; Brown3 (Warning)
       
       ;; Magit (Earthy diffs)
       (diff-add          "#d4e4d4")
       (diff-rem          "#e4d4d4")
       
       ;; UI
       (cursor-col        "#5c4033")
       (mode-active-bg    "#5c4033") ; Dark Brown
       (mode-active-fg    "#eecbad") ; Peach
       (mode-inactive-bg  "#cdaa7d")
       (mode-inactive-fg  "#3b2f2f"))

  (custom-theme-set-faces
   'kj-clay

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
   
   `(link ((t (:foreground ,color-clay :underline t))))
   
   ;; --- Line Numbers ---
   `(line-number ((t (:foreground "#a0522d" :background ,bg-alt))))
   `(line-number-current-line ((t (:background ,bg-hl :foreground ,color-clay :weight bold))))

   ;; --- Syntax (Organic Focus) ---
   `(font-lock-comment-face ((t (:foreground ,fg-dim :slant italic))))
   `(font-lock-keyword-face ((t (:foreground ,color-clay :weight bold)))) 
   `(font-lock-string-face ((t (:foreground ,color-stone))))
   `(font-lock-constant-face ((t (:foreground ,color-brick :weight bold))))
   `(font-lock-builtin-face ((t (:foreground ,color-sand))))
   `(font-lock-type-face ((t (:foreground ,color-sand :weight bold))))
   `(font-lock-function-name-face ((t (:foreground ,color-clay :weight bold))))
   `(font-lock-variable-name-face ((t (:foreground ,color-brick :weight bold))))
   `(font-lock-warning-face ((t (:foreground ,color-alert :weight bold))))
   `(font-lock-doc-face ((t (:foreground ,fg-dim))))

   ;; --- ORG MODE (The Sculpture) ---
   `(org-hide ((t (:foreground ,bg-main))))
   
   ;; Headings: Earthy Gradient
   `(org-level-1 ((t (:foreground "#5c4033" :weight bold :height 1.3)))) ; Darkest Brown
   `(org-level-2 ((t (:foreground "#8b4513" :weight bold :height 1.15)))) ; SaddleBrown
   `(org-level-3 ((t (:foreground "#a0522d" :weight bold)))) ; Sienna
   `(org-level-4 ((t (:foreground "#8b7355" :weight bold))))
   
   `(org-block ((t (:background ,bg-block :foreground ,fg-main :extend t))))
   `(org-block-begin-line ((t (:foreground ,fg-dim :background ,bg-block :slant italic))))
   `(org-block-end-line ((t (:foreground ,fg-dim :background ,bg-block :slant italic))))
   `(org-document-title ((t (:foreground ,color-clay :weight bold :height 1.5))))
   
   ;; Todos
   `(org-todo ((t (:foreground "#eecbad" :background "#8b2323" :weight bold :box (:line-width 1 :style nil)))))
   `(org-done ((t (:foreground "#556b2f" :weight bold))))
   `(org-headline-done ((t (:foreground ,fg-dim :strike-through t))))

   ;; --- ORG AGENDA ---
   `(org-agenda-date ((t (:foreground ,color-clay :weight bold :height 1.1))))
   `(org-agenda-date-today ((t (:foreground ,color-clay :weight bold :underline t :height 1.1))))
   `(org-agenda-structure ((t (:foreground ,color-brick :weight bold :height 1.2))))
   `(org-upcoming-deadline ((t (:foreground ,color-alert :weight bold))))
   `(org-scheduled-today ((t (:foreground ,color-clay :weight bold))))
   
   ;; --- TOOLS ---
   `(dired-directory ((t (:foreground ,color-clay :weight bold))))
   `(dired-symlink ((t (:foreground ,color-brick :slant italic))))
   `(dired-marked ((t (:background ,bg-region :foreground ,fg-main :weight bold))))
   
   `(magit-section-heading ((t (:foreground ,color-clay :weight bold))))
   `(magit-diff-added ((t (:background ,diff-add :foreground "#2e8b57"))))
   `(magit-diff-removed ((t (:background ,diff-rem :foreground "#a0522d"))))
   `(magit-header-line ((t (:background ,mode-active-bg :foreground ,mode-active-fg))))
   
   ;; --- Mode Line ---
   `(mode-line ((t (:background ,mode-active-bg :foreground ,mode-active-fg
                    :box (:line-width 1 :color ,mode-active-bg :style nil)))))
   `(mode-line-inactive ((t (:background ,mode-inactive-bg :foreground ,mode-inactive-fg
                             :box (:line-width 1 :color ,mode-inactive-bg :style nil)))))
   
   `(isearch ((t (:background "#ffa500" :foreground "#000000"))))
   ))

(add-hook 'prog-mode-hook 'hl-line-mode)
(setq-default cursor-type 'box)
(provide-theme 'kj-clay)
