;;; kj-theme.el --- A fresh honeydew-based color theme  -*- lexical-binding: t; -*-
;; Copyright (C) 2025 [Your Name]

(deftheme kj "A fresh honeydew-based color theme")

;; Color palette
(let* (;; Backgrounds
       (bg-main           "#f0fff0") ; Honeydew
       (bg-alt            "#e4f4e4") ; Slightly darker honeydew for fringes/lines
       (bg-hl             "#dcf0dc") ; Highlight line
       (bg-region         "#cceacc") ; Selection region
       (bg-code-block     "#e2f2e2") ; Specific bg for org blocks
       
       ;; Foreground / Text
       (fg-main           "#384039") ; Dark charcoal with a hint of green
       (fg-dim            "#7fa085") ; Dimmed text (lighter grey-green)
       (fg-meta           "#9aa")    ; Meta info (org #+TITLE etc)
       
       ;; The Blues (Requested)
       (blue-deep         "#2f4f6f") ; Dark Slate Blue (Keywords/Level 1)
       (blue-vibrant      "#006688") ; Teal Blue (Variables)
       (blue-sky          "#4da6ff") ; Light blue accents
       (blue-muted        "#5f87a6") ; Muted blue
       
       ;; Accents (Low contrast/Earthy)
       (green-pine        "#2e8b57") ; Seagreen
       (green-olive       "#556b2f") ; Olive
       (brown-clay        "#a0522d") ; Sienna (Level 2)
       (orange-burnt      "#cc7a00") ; Muted orange
       (red-soft          "#cc5555") ; Soft red
       (purple-dusty      "#8f5f8f") ; Muted purple (Level 3)
       
       ;; UI Specific
       (cursor-color      "#006688")
       (modeline-active-bg "#405045")
       (modeline-active-fg "#f0fff0")
       (modeline-inactive-bg "#c0d0c0")
       (modeline-inactive-fg "#384039")
       (border            "#aabdaa"))

  (custom-theme-set-faces
   'kj

   ;; --- UI Elements ---
   `(default ((t (:background ,bg-main :foreground ,fg-main))))
   `(cursor ((t (:background ,cursor-color))))
   `(region ((t (:background ,bg-region :extend t))))
   `(highlight ((t (:background ,bg-hl :extend t))))
   `(hl-line ((t (:background ,bg-hl :extend t))))
   `(fringe ((t (:background ,bg-alt :foreground ,fg-dim))))
   `(vertical-border ((t (:foreground ,border))))
   `(minibuffer-prompt ((t (:foreground ,blue-vibrant :weight bold))))
   `(link ((t (:foreground ,blue-vibrant :underline t))))
   
   ;; --- Line Numbers ---
   `(line-number ((t (:foreground ,fg-dim :background ,bg-alt))))
   `(line-number-current-line ((t (:background ,bg-hl :foreground ,blue-deep :weight bold))))

   ;; --- Font Lock (Code Syntax) ---
   `(font-lock-comment-face ((t (:foreground ,fg-dim :slant italic))))
   `(font-lock-keyword-face ((t (:foreground ,blue-deep :weight bold))))
   `(font-lock-string-face ((t (:foreground ,green-olive))))
   `(font-lock-constant-face ((t (:foreground ,green-pine :weight bold))))
   `(font-lock-builtin-face ((t (:foreground ,purple-dusty))))
   `(font-lock-type-face ((t (:foreground ,brown-clay))))
   `(font-lock-function-name-face ((t (:foreground ,blue-deep :weight bold))))
   `(font-lock-variable-name-face ((t (:foreground ,blue-vibrant :weight bold))))
   `(font-lock-preprocessor-face ((t (:foreground ,blue-muted))))
   `(font-lock-warning-face ((t (:foreground ,red-soft :weight bold))))
   `(font-lock-doc-face ((t (:foreground ,fg-dim))))

   ;; --- ORG MODE SPECIFIC (Tuned) ---
   
   ;; 1. Document Title: Very Large, Teal
   `(org-document-title ((t (:foreground ,blue-vibrant :weight bold :height 1.5))))
   
   ;; 2. Meta lines (#+TITLE, #+AUTHOR): Faint, distinct from content
   `(org-document-info ((t (:foreground ,fg-meta))))
   `(org-meta-line ((t (:foreground ,fg-meta))))
   
   ;; 3. Headings: Distinct Colors and Heights
   ;; Level 1: Deep Blue, Large (1.3x)
   `(org-level-1 ((t (:foreground ,blue-deep :weight bold :height 1.3 :inherit outline-1))))
   
   ;; Level 2: Earthy Clay/Brown, Medium (1.15x) - High contrast from Blue
   `(org-level-2 ((t (:foreground ,brown-clay :weight bold :height 1.15 :inherit outline-2))))
   
   ;; Level 3: Dusty Purple, Normal Size
   `(org-level-3 ((t (:foreground ,purple-dusty :weight bold :inherit outline-3))))
   
   ;; Level 4: Olive Green
   `(org-level-4 ((t (:foreground ,green-olive :weight bold :inherit outline-4))))
   
   ;; 4. Code Blocks: Subtle background box to separate from prose
   `(org-block ((t (:background ,bg-code-block :foreground ,fg-main :extend t))))
   `(org-block-begin-line ((t (:foreground ,fg-dim :background ,bg-code-block :slant italic))))
   `(org-block-end-line ((t (:foreground ,fg-dim :background ,bg-code-block :slant italic))))
   
   ;; 5. Todo/Done keywords
   `(org-todo ((t (:foreground ,red-soft :weight bold))))
   `(org-done ((t (:foreground ,green-pine :weight bold))))
   `(org-headline-done ((t (:foreground ,fg-dim :strike-through t))))

   ;; --- Mode Line ---
   ;; Replace the "Mode Line" section in the theme with this:
   `(mode-line ((t (:background "#5f87a6"  ; Steel Blue
				:foreground "#ffffff"  ; White text
				:box (:line-width 1 :color "#5f87a6" :style nil)))))
   `(mode-line-inactive ((t (:background "#d0dbe6" ; Light Blueish-Grey
					 :foreground "#5f6f7f"
					 :box (:line-width 1 :color "#d0dbe6" :style nil)))))

   ;; --- Search & Compilation ---
   `(isearch ((t (:background ,orange-burnt :foreground "#ffffff"))))
   `(lazy-highlight ((t (:background ,bg-hl :foreground ,orange-burnt :weight bold))))
   `(show-paren-match ((t (:background ,blue-muted :foreground "#ffffff"))))
   `(show-paren-mismatch ((t (:background ,red-soft :foreground "#ffffff"))))
   `(error ((t (:foreground ,red-soft :weight bold))))
   `(warning ((t (:foreground ,orange-burnt))))
   `(success ((t (:foreground ,green-pine))))
   ))

;; --- Extras ---
(add-hook 'prog-mode-hook 'hl-line-mode)

(setq-default cursor-type 'box)


(provide-theme 'kj)
