;;; miniflux.el --- A simple connection to the Miniflux RSS Aggregator -*- lexical-binding: t; -*-

;; Author: Adam Washington <rprospero@gmail.com>
;; Keywords: comm rss
;; Package-Requires: ((emacs "29.1") (request "0.3.3"))
;; Version: 0.8.92

;;; Commentary:
;;; Code:

(require 'hierarchy)
(require 'parse-time)
(require 'request)


(defgroup miniflux nil
  "Access to the Miniflux feed aggregator through Emacs."
  :group 'applications
  :link
  '(url-link "https://miniflux.app/"))

(defcustom miniflux-token ""
  "The API token for the miniflux server."
  :type '(string)
  :link
  '(url-link "https://miniflux.app/docs/api.html#authentication")
  :group 'miniflux)

(defcustom miniflux-server "https://example.com/v1"
  "The url for the miniflux API on the server."
  :type '(string)
  :group 'miniflux)

(defface miniflux-keyword-face '((t :inherit font-lock-keyword-face))
  "The face for the keywords on a feed entry (e.g. Title)."
  :group 'miniflux)

(defface miniflux-value-face '((t :inherit font-lock-string-face))
  "The face for the keyword values on a feed entry (e.g. the author)."
  :group 'miniflux)

(defvar-local miniflux--feed -1
  "The feed identity for the current buffer.")

(defvar miniflux--unread-counts '()
  "The number of unread entries for each feed.")

(defvar miniflux--read-counts '()
  "The number of previously read entries for each feed.")

(defvar-local miniflux--entry-id -1
    "The id for this entry in miniflux")

(defvar-local miniflux--external-link ""
    "The external link for this entry in miniflux")

(defun miniflux--or-else (value default)
  "Returns the VALUE, except when it is nil and uses the DEFAULT."
  (if (eq value nil) default value))

(define-derived-mode miniflux-feed-mode tabulated-list-mode "miniflux-feed-mode"
  "Major mode for Miniflux feed."
  (setq tabulated-list-format [
			       ("Published" 14 nil)
			       ("🌟"  2 t nil)
			       ("✉"  2 t nil)
			       ("Reading Time" 15 nil)
			       ("Title" 0 t);; last columnt takes what left
			       ])

  (setq tabulated-list-padding 4)
  (setq tabulated-list-sort-key (cons "Published" nil))
  (add-hook 'tabulated-list-revert-hook
	    (lambda ()
	      (miniflux-get-feed miniflux--feed)))
  (tabulated-list-init-header)
  (tabulated-list-print t))

(defun miniflux--get-stats()
  "Get the read and unread counts for the feeds"
  (request
    (format "%s/feeds/counters" miniflux-server)
    :headers (list (cons "X-Auth-Token" miniflux-token))
    :parser 'json-read
    :sync? t
    :success
    (cl-function
     (lambda (&key data &allow-other-keys)
       (let ((fixer (lambda (x) (cons (read (symbol-name (car x))) (cdr x)))))
	 (setq miniflux--unread-counts (mapcar fixer (assoc-default 'unreads data)))
	 (setq miniflux--read-counts (mapcar fixer (assoc-default 'reads data))))))))


(define-derived-mode miniflux-entry-mode special-mode "miniflux-entry-mode"
  "Major mode for Miniflux entries.")

(bind-key "o" 'miniflux-entry-follow-external-link miniflux-entry-mode-map)
(bind-key "q" 'kill-this-buffer miniflux-entry-mode-map)
(bind-key "<normal-state> o" 'miniflux-entry-follow-external-link miniflux-entry-mode-map)
(bind-key "<normal-state> q" 'kill-this-buffer miniflux-entry-mode-map)

(defun miniflux-entry-follow-external-link ()
  "Open the external link for the entry"
  (interactive)
  (message "Following %s" miniflux--external-link)
  (browse-url miniflux--external-link))

(defun miniflux--update-entry (data)
  "Display the entry DATA from a miniflux Json result in a buffer."
  (miniflux--mark-entry-as-read (assoc-default 'id data))
  (pop-to-buffer "*MINIFLUX-ENTRY*" nil)
  (erase-buffer)
  (insert
   (propertize "Title: " 'face 'miniflux-keyword-face)
   (propertize (assoc-default 'title data) 'face 'miniflux-value-face)
   "\n"
   (propertize "Date: " 'face 'miniflux-keyword-face)
   (propertize (assoc-default 'published_at data) 'face 'miniflux-value-face)
   "\n"
   (propertize "Author: " 'face 'miniflux-keyword-face)
   (propertize (assoc-default 'author data) 'face 'miniflux-value-face)
   "\n\n")
  (save-excursion
    (insert (assoc-default 'content data)))
  (letrec ((start (point))
	   (dom (libxml-parse-html-region start (point-max))))
    (delete-region start (point-max))
    (shr-insert-document dom))
  (miniflux-entry-mode)
  (setq miniflux--entry-id (assoc-default 'id data))
  (setq miniflux--external-link (assoc-default 'url data)))

(defun miniflux-entry ()
  "Display the miniflux entry under the point."
  (interactive)
  (request
    (format "%s/entries/%d" miniflux-server (tabulated-list-get-id))
    :headers (list (cons "X-Auth-Token" miniflux-token))
    :parser 'json-read
    :sync? t
    :success
    (cl-function
     (lambda (&key data &allow-other-keys)
       (miniflux--update-entry data)))))

(defun miniflux-mark-entry-as-read ()
  "Mark the entry under the point as read."
  (interactive)
  (miniflux--mark-entry-as-read (tabulated-list-get-id))
  (next-line))

(defun miniflux--mark-entry-as-read (entry)
  "Mark a given ENTRY as read."
  (request
    (format "%s/entries" miniflux-server)
    :type "PUT"
    :headers (list (cons "X-Auth-Token" miniflux-token) '("Content-Type" . "application/json"))
    :data (json-encode (list (cons "entry_ids" (list entry)) '("status" . "read")))
    :parser 'json-read
    :sync? t
    :success (cl-function
	      (lambda (&key data &allow-other-keys) (revert-buffer)))
    :error
    (cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
		   (message "Got error: %S" error-thrown)))))

(bind-key "<return>" 'miniflux-entry miniflux-feed-mode-map)
(bind-key "r" 'miniflux-mark-entry-as-read miniflux-feed-mode-map)
(bind-key "<normal-state> r" 'miniflux-mark-entry-as-read miniflux-feed-mode-map)

(defun miniflux-feed (feed)
  "Open a buffer listing the entries in a given FEED."
  (pop-to-buffer "*MINIFLUX-FEED*" nil)
  (setq miniflux--feed feed)
  (miniflux-get-feed feed)
  (miniflux-feed-mode))

(defun miniflux--time-diff (time)
  "Format the relative distance between this moment and a given TIME in the past."
  (letrec ((then (encode-time (parse-time-string time)))
	   (now (encode-time (decode-time)))
	   (diff (+ (* (expt 2 16) (- (car now) (car then)))
		    (- (cadr now) (cadr then))
		    )))
    (cond
     ((>= diff (* 3600 24 365 2))
      (format "%s years" (floor diff (* 3600 24 365))))
     ((> diff (* 3600 24 365))
      (format "%s year" (floor diff (* 3600 24 365))))
     ((>= diff (* 3600 24 30 2))
      (format "%s months" (floor diff (* 3600 24 30))))
     ((> diff (* 3600 24 30))
      (format "%s month" (floor diff (* 3600 24 30))))
     ((>= diff (* 3600 24 2))
      (format "%s days" (floor diff (* 3600 24))))
     ((> diff (* 3600 24))
      (format "%s day" (floor diff (* 3600 24))))
     ((>= diff (* 60 60 2))
      (format "%s hours" (floor diff (* 60 60))))
     ((> diff (* 60 60))
      (format "%s hour" (floor diff (* 60 60))))
     ((>= diff 60 2)
      (format "%s minutes" (floor diff 60)))
     ((> diff 60)
      (format "%s minute" (floor diff 60)))
     (t
      (format "%s seconds" diff)))))

(defun miniflux--update-feed (data)
  "Display the feed DATA from a miniflux Json result in a buffer."
  (setq tabulated-list-entries
	(mapcar
	 (lambda (x)
	   (list
	    (assoc-default 'id x)
	    (vector
	     (format "%s ago" (miniflux--time-diff (assoc-default 'published_at x)))
	     (if (eq (assoc-default 'starred x) :json-false) " " "⭐")
	     (if (string= (assoc-default 'status x) "unread") "✉" " ")
	     (format "%d min" (assoc-default 'reading_time x))
	     (assoc-default 'title x))))
	 (assoc-default 'entries data)))
  (tabulated-list-print t))

(defun miniflux-get-feed (feed)
  "Display the miniflux FEED under the point."
  (request
    (format "%s/feeds/%d/entries" miniflux-server feed)
    :headers (list (cons "X-Auth-Token" miniflux-token))
    :params '(("status" . "unread"))
    :parser 'json-read
    :sync? t
    :success (cl-function
	      (lambda (&key data &allow-other-keys)
		(miniflux--update-feed data)))))

(defun miniflux--update-feeds (data)
  "Display the category and feed DATA from a miniflux Json result in a buffer."
  (mapcar
   (lambda (x)
     (hierarchy-add-tree miniflux-root
			 x
			 (lambda (x)
			   (cond
			    ((eq x 'miniflux) 'nil)
			    ((stringp x) 'miniflux)
			    ((assoc 'category x)
			     (assoc-default
			      'title
			      (assoc-default 'category x)))))))
   data)

  ;; Create display buffer
  (switch-to-buffer
   (hierarchy-tree-display
    miniflux-root
    (hierarchy-labelfn-button-if
     (lambda (x count)
       (insert
	(format "%s"
		(cond
		 ((not x) "Failed")
		 ((eq x 'miniflux) "Miniflux")
		 ((stringp x)
		  (format "%s (%s)" x
			  (apply #'+
			  (mapcar
			   (lambda (child)
			     (miniflux--or-else
			      (assoc-default (assoc-default 'id child) miniflux--unread-counts) 0))
			   (hierarchy-children miniflux-root x)))))
		 ((and (assoc 'title x) (assoc 'id x))
		  (format "%s (%s)" (assoc-default 'title x)
			  (miniflux--or-else
			   (assoc-default (assoc-default 'id x) miniflux--unread-counts) 0)))))))
     (lambda (x count)
       (cond
	((symbolp x) 'nil)
	((stringp x) 'nil)
	((assoc 'title x) t)))
     (lambda (x count)
       (miniflux-feed (assoc-default 'id x)))))))

(defun miniflux-init ()
  "Connect to a Miniflux server."
  (interactive)

  (setq miniflux-root (hierarchy-new))

  (miniflux--get-stats)

  ;; Populate miniflux feeds list
  (request
    (format "%s/feeds" miniflux-server)
    :headers (list (cons "X-Auth-Token" miniflux-token))
    :parser 'json-read
    :sync? t
    :success
    (cl-function
     (lambda (&key data &allow-other-keys)
       (miniflux--update-feeds data)))))

(provide 'miniflux)
;;; miniflux.el ends here
