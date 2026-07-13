;;; kj-sage-theme.el --- A focus-optimized sage theme with full tool support
;; Copyright (C) 2025 [Your Name]

;; Commentary:
;; A complete theme for focus-heavy workflows.
;; - Background: Sage Green (#cfdfcf) to reduce iris fatigue.
;; - Palette: Cool Purples, Teals, and Earthy Reds.
;; - Support: Org Mode (Modern), Org Agenda, Dired, Magit.
;; - Cursor: Consistent Block cursor.
;; - Fixes: Olivetti mode margins blend seamlessly.

(deftheme kj-sage "A focus-optimized sage-green theme")

(let* ((bg-main           "#cfdfcf") ; Sage
       (bg-alt            "#c4d4c4") ; Mode line inactive / Line number column
       (bg-hl             "#bfe0bf") ; Highlight line
       (bg-region         "#d8bfd8") ; Thistle (Selection)
       (bg-block          "#c1d1c1") ; Code blocks
       
       ;; Text
       (fg-main           "#1a261a") ; Dark Forest (Main text)
       (fg-dim            "#506050") ; Dimmed Slate (Comments/Metadata)
       
       ;; The Palette
       (color-keyword     "#483d8b") ; DarkSlateBlue
       (color-var         "#004d4d") ; Deep Teal
       (color-str         "#8b2252") ; VioletRed4
       (color-type        "#2f4f4f") ; DarkSlateGray
       (color-func        "#4b0082") ; Indigo
       (color-const       "#191970") ; MidnightBlue
       (color-warning     "#8b4500") ; Dark Orange (Deadlines)
       (color-error       "#8b0000") ; Deep Red (Errors/Overdue)
       (color-success     "#2e8b57") ; SeaGreen (Done/Added)

       ;; Magit Specific Backgrounds
       (magit-diff-add    "#d4e4d4")
       (magit-diff-rem    "#e4d4d4")
       (magit-diff-hunk   "#c4c4d4")
       
       ;; UI
       (cursor-col        "#2f4f4f")
       (mode-active-bg    "#4a5d4a") 
       (mode-active-fg    "#f0fff0")
       (mode-inactive-bg  "#a9b9a9")
       (mode-inactive-fg  "#1a261a"))

  (custom-theme-set-faces
   'kj-sage

   ;; --- Basics ---
   `(default ((t (:background ,bg-main :foreground ,fg-main))))
   `(cursor ((t (:background ,cursor-col))))
   `(region ((t (:background ,bg-region :extend t))))
   `(highlight ((t (:background ,bg-hl :extend t))))
   `(hl-line ((t (:background ,bg-hl :extend t))))
   
   ;; --- THE FIX FOR OLIVETTI MODE ---
   ;; Fringe must match bg-main so it disappears in distraction-free modes
   `(fringe ((t (:background ,bg-main :foreground ,fg-dim))))
   ;; Vertical border matches background to be invisible in single-window usage
   `(vertical-border ((t (:foreground ,bg-alt :background ,bg-main))))
   ;; Window divider (if used) also matches
   `(window-divider ((t (:foreground ,bg-alt :background ,bg-main))))
   
   `(link ((t (:foreground ,color-func :underline t))))
   
   ;; --- Line Numbers ---
   ;; We keep the darker background specifically for the number column itself
   `(line-number ((t (:foreground "#7f8f7f" :background ,bg-alt))))
   `(line-number-current-line ((t (:background ,bg-hl :foreground ,color-keyword :weight bold))))

   ;; --- Syntax Highlighting ---
   `(font-lock-comment-face ((t (:foreground ,fg-dim :slant italic))))
   `(font-lock-keyword-face ((t (:foreground ,color-keyword :weight bold))))
   `(font-lock-string-face ((t (:foreground ,color-str))))
   `(font-lock-constant-face ((t (:foreground ,color-const :weight bold))))
   `(font-lock-builtin-face ((t (:foreground "#8b4789"))))
   `(font-lock-type-face ((t (:foreground ,color-type :weight bold))))
   `(font-lock-function-name-face ((t (:foreground ,color-func :weight bold))))
   `(font-lock-variable-name-face ((t (:foreground ,color-var :weight bold))))
   `(font-lock-preprocessor-face ((t (:foreground "#483d8b"))))
   `(font-lock-warning-face ((t (:foreground ,color-error :weight bold))))
   `(font-lock-doc-face ((t (:foreground ,fg-dim))))

   ;; --- ORG MODE ---
   `(org-hide ((t (:foreground ,bg-main)))) ; Fixes leading asterisks
   `(org-level-1 ((t (:foreground "#4b0082" :weight bold :height 1.3))))
   `(org-level-2 ((t (:foreground "#004d4d" :weight bold :height 1.15))))
   `(org-level-3 ((t (:foreground "#8b2252" :weight bold))))
   `(org-level-4 ((t (:foreground "#2f4f4f" :weight bold))))
   
   `(org-block ((t (:background ,bg-block :foreground ,fg-main :extend t))))
   `(org-block-begin-line ((t (:foreground ,fg-dim :background ,bg-block :slant italic))))
   `(org-block-end-line ((t (:foreground ,fg-dim :background ,bg-block :slant italic))))
   `(org-document-title ((t (:foreground ,color-func :weight bold :height 1.5))))
   `(org-document-info ((t (:foreground ,fg-dim))))
   
   `(org-todo ((t (:foreground ,color-error :weight bold))))
   `(org-done ((t (:foreground ,color-success :weight bold))))
   `(org-headline-done ((t (:foreground ,fg-dim :strike-through t :weight normal))))

   ;; --- ORG AGENDA ---
   `(org-agenda-date ((t (:foreground ,color-func :weight bold :height 1.1))))
   `(org-agenda-date-today ((t (:foreground ,color-func :weight bold :underline t :height 1.1))))
   `(org-agenda-structure ((t (:foreground ,color-var :weight bold :height 1.2))))
   `(org-upcoming-deadline ((t (:foreground ,color-warning :weight bold))))
   `(org-upcoming-distance ((t (:foreground ,fg-dim :weight bold))))
   `(org-scheduled ((t (:foreground ,color-type))))
   `(org-scheduled-today ((t (:foreground ,color-var :weight bold))))
   `(org-time-grid ((t (:foreground ,fg-dim))))
   `(org-agenda-done ((t (:foreground ,fg-dim :strike-through t))))

   ;; --- ORG MODERN ---
   `(org-modern-tag ((t (:background "#a9b9a9" :foreground "black" :box nil))))
   `(org-modern-date-active ((t (:background ,bg-alt :foreground "black"))))
   `(org-modern-time-active ((t (:background ,bg-alt :foreground "black"))))
   
   ;; --- DIRED ---
   `(dired-directory ((t (:foreground ,color-func :weight bold))))
   `(dired-symlink ((t (:foreground ,color-var :slant italic))))
   `(dired-perm-write ((t (:foreground ,color-error))))
   `(dired-marked ((t (:background ,bg-region :foreground ,fg-main :weight bold))))
   `(dired-header ((t (:foreground ,color-type :weight bold))))

   ;; --- MAGIT ---
   `(magit-section-heading ((t (:foreground ,color-keyword :weight bold))))
   `(magit-section-highlight ((t (:background ,bg-hl :extend t))))
   `(magit-branch-local ((t (:foreground ,color-var :weight bold))))
   `(magit-branch-remote ((t (:foreground ,color-func :slant italic))))
   `(magit-diff-added ((t (:background ,magit-diff-add :foreground ,color-success))))
   `(magit-diff-added-highlight ((t (:background "#c4e4c4" :foreground ,color-success :weight bold))))
   `(magit-diff-removed ((t (:background ,magit-diff-rem :foreground ,color-error))))
   `(magit-diff-removed-highlight ((t (:background "#e4c4c4" :foreground ,color-error :weight bold))))
   `(magit-diff-hunk-heading ((t (:background ,magit-diff-hunk :foreground ,fg-dim))))
   `(magit-diff-hunk-heading-highlight ((t (:background "#b0b0c0" :foreground ,fg-main :weight bold))))
   `(magit-header-line ((t (:background ,mode-active-bg :foreground ,mode-active-fg))))
   
   ;; --- Mode Line ---
   `(mode-line ((t (:background ,mode-active-bg :foreground ,mode-active-fg
                    :box (:line-width 1 :color ,mode-active-bg :style nil)))))
   `(mode-line-inactive ((t (:background ,mode-inactive-bg :foreground ,mode-inactive-fg
                             :box (:line-width 1 :color ,mode-inactive-bg :style nil)))))

   ;; --- Search ---
   `(isearch ((t (:background "#dda0dd" :foreground "#000000")))) 
   `(lazy-highlight ((t (:background "#e6e6fa" :foreground "#000000"))))
   ))

(add-hook 'prog-mode-hook 'hl-line-mode)
(setq-default cursor-type 'box)

(provide-theme 'kj-sage)
