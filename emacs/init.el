
;; UI Elements
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;;Package.el
(require 'package)

;; Add third-party package repositories
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/" ) t)
(add-to-list 'package-archives '("nongnu" . "https://nongnu.org" ) t)

;; Initialize the package system
(package-initialize)

;; Fetch the package list from archives if not already done
(unless package-archive-contents
  (package-refresh-contents))


(add-to-list 'default-frame-alist '(background-color . "honeydew"))

;; Latex Keybinds

;; Enable Abbrev Mode automatically when opening a LaTeX file
(add-hook 'latex-mode-hook 'abbrev-mode)

(with-eval-after-load 'latex-mode
  ;; 1. INLINE EXPANSIONS (Abbrevs)
  ;; Type the shortcut followed by Space or Punctuation to expand
  (define-abbrev latex-mode-abbrev-table "tbf" "\\textbf{}")
  (define-abbrev latex-mode-abbrev-table "tit" "\\textit{}")
  (define-abbrev latex-mode-abbrev-table "mit" "\\item")

  ;; 2. STRUCTURAL TEMPLATES (Skeletons)
  
  ;; Your Custom Main Document Template
  (define-skeleton latex-main-template
    "Inserts Krishna's custom xelatex template."
    nil
    "% Created " (format-time-string "%Y-%m-%d %a %H:%M") "\n"
    "% Intended LaTeX compiler: xelatex\n"
    "\\documentclass[12pt]{article}\n"
    "\\usepackage[utf8]{inputenc}\n"
    "\\usepackage[T1]{fontenc}\n"
    "\\usepackage{graphicx}\n"
    "\\usepackage{longtable}\n"
    "\\usepackage{wrapfig}\n"
    "\\usepackage[margin=1in]{geometry}\n"
    "\\usepackage{rotating}\n"
    "\\usepackage{setspace}\n"
    "\\onehalfspacing\n"
    "\\usepackage[normalem]{ulem}\n"
    "\\usepackage{amsmath}\n"
    "\\usepackage{amssymb}\n"
    "\\usepackage{capt-of}\n"
    "\\setcounter{secnumdepth}{4} % Tells LaTeX to number down to paragraphs\n"
    "\\setcounter{tocdepth}{4}  \n"
    "\\usepackage{hyperref}\n"
    "\\usepackage[style=oscola, backend=biber]{biblatex}\n"
    "\\addbibresource{/Users/krishnajani/Documents/Libib.bib}\n"
    "\\usepackage{csquotes}\n"
    "\\usepackage[british]{babel}\n"
    "\\setlength{\\parskip}{0.5\\baselineskip}\n"

    "\\author{Krishna Jani}\n"
    "\\date{\\today}\n"
    "\\title{" (skeleton-read "Document Title: ") "}\n\n"
    "\\begin{document}\n"
    "\\maketitle\n\n"
    _ "\n\n"
    "\\end{document}")
  
  ;; Enumerate List Template
  (define-skeleton latex-enumerate-template
    "Inserts an enumerate environment with a starting item."
    nil
    "\\begin{enumerate}\n"
    "  \\item " _ "\n"
    "\\end{enumerate}")

  ;; 3. KEYBINDINGS FOR SKELETONS
  ;; Maps the templates to easy keyboard shortcuts in LaTeX mode
  (define-key latex-mode-map (kbd "C-c t") 'latex-main-template)
  (define-key latex-mode-map (kbd "C-c n") 'latex-enumerate-template))

(vertico-mode 1)


(if after-init-time
    (edit-server-start)
  (add-hook 'after-init-hook
            #'(lambda() (edit-server-start))))
(setq edit-server-new-frame-alist
      '((name . "Edit with Emacs FRAME")
        (top . 200)
        (left . 200)
        (width . 80)
        (height . 25)
        (minibuffer . t)
        (menu-bar-lines . t)
        (window-system . x)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(citar-bibliography '("/Users/krishnajani/Documents/Libib.bib"))
 '(package-selected-packages '(auctex citar edit-server magit vertico)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
