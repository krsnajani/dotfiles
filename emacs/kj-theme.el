;;; xah-norm-theme.el --- custom theme for faces  -*- lexical-binding:t -*-

;; Copyright (C) 2022 by you.

;; Author: you <your_email_address>

;;; Commentary:

;; a beautiful theme

;;; Code:

(deftheme kj
  "Just a honey background and bold var names.")

(custom-theme-set-faces
 'kj
 '(default ((t (:background "honeydew"))))
 '(font-lock-constant-face ((t (:foreground "green" :bold t))))
 '(font-lock-variable-name-face ((t (:foreground "blue" :bold t))))
 ;;
 )

(provide-theme 'kj)

;;; xah-norm-theme.el ends here
