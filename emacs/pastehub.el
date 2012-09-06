;; -*- coding: utf-8 -*-
;;
;; "pastehub.el" is a client program for PasteHub cloud service
;;
;;   Copyright (C) 2012 Kiyoka Nishiyama
;;

(defcustom pastehub-client-post "pastehub-clientPost"
  "client (post) program name."
  :type  'string
  :group 'pastehub)

(defcustom pastehub-client-dump "pastehub-clientDump"
  "client (dump) program name."
  :type  'string
  :group 'pastehub)

(defcustom pastehub-sync-items 10
  "number of paste items to sync."
  :type 'integer
  :group 'pastehub)


(defvar pastehub-latest-date ""         "latest synced date.")
(defvar pastehub-timer-object nil       "interval timer object.")
(defvar pastehub-unread-count 0         "number of unread pastes")

(defvar pastehub-sync-cache   '()       "cache of key-value")

(defvar pastehub-mode nil               "pastehub toggle for mode")
(defun pastehub-modeline-string ()
  ;; display unread count
  (format " PasteHub[%d]" pastehub-unread-count))

(or (assq 'pastehub-mode minor-mode-alist)
    (setq minor-mode-alist (cons
			    '(pastehub-mode (:eval (pastehub-modeline-string)))
			    minor-mode-alist)))

;;
;; Paste
;;
(defun posthub-post-internal ()
  (when (car kill-ring)
    (with-temp-buffer
      (insert (substring-no-properties (car kill-ring)))
      (call-process-region (point-min) (point-max)
			   pastehub-client-post))))

(defadvice kill-new (after pastehub-post activate)
  "Post the latest killed text to pastehub cloud service."
  (posthub-post-internal))

(ad-activate 'kill-new)

(defadvice insert-for-yank-1 (after pastehub-insert-for-yank-1 activate)
  "reset unread counter."
  (setq pastehub-unread-count 0))
  
(ad-activate 'insert-for-yank-1)


;;
;; Poll & Sync
;;
(defun pastehub-call-process (process-name arg1 arg2)
  "call-process and return output string of the command."
  (let ((outbuf (get-buffer-create "*pastehub output*")))
    (with-current-buffer (buffer-name outbuf)
      (delete-region (point-min) (point-max)))
    (with-temp-buffer
      (call-process process-name
		    nil ;; infile
		    outbuf
		    nil ;; display
		    arg1
		    arg2))
    (with-current-buffer (buffer-name outbuf)
      (let ((result-str (buffer-substring-no-properties (point-min) (point-max))))
	result-str))))

(defun pastehub-get-value (key)
  "get value from localDB. ( cache feature built-in )"
  (let ((pair (assoc key pastehub-sync-cache)))
    (cond 
     (pair
      ;;(message (format "%s:%s" (car pair) (cdr pair)))
      (cdr pair))
     (t
      (let ((value (pastehub-call-process pastehub-client-dump "get" key)))
	(setq pastehub-sync-cache
	      (cons
	       (cons key value)
	       pastehub-sync-cache))
	value)))))

;; like scheme's `take'
(defun pastehub-take (lst n)
  (reverse (last (reverse lst)
		 n)))

(defun pastehub-sync-kill-ring ()
  "sync kill-ring"
  (message "syncing kill-ring...")
  (let* ((keys-string (pastehub-call-process pastehub-client-dump "list" (format "%d" pastehub-sync-items)))
	 (keys
	  (pastehub-take (split-string 
			  keys-string
			  "\n")
			 pastehub-sync-items)))
    (let ((old (car kill-ring)))
      (setq kill-ring
	    (mapcar
	     (lambda (key)
	       (pastehub-get-value key))
	     keys))
      (setq kill-ring-yank-pointer kill-ring)
      (when (and old (car kill-ring))
	(when (not (string-equal old (car kill-ring)))
	  (setq pastehub-unread-count 
		(+ pastehub-unread-count 1))))))
  (message nil))


(defun pastehub-timer-handler ()
  "polling process handler for pastehub service."
  (let ((latest-date
	 (pastehub-call-process pastehub-client-dump "latest" "")))
    (if (not (string-equal pastehub-latest-date latest-date))
	(progn
	  (setq pastehub-latest-date latest-date)
	  (pastehub-sync-kill-ring)))))
	    
(defun pastehub-sigusr-handler ()
  (interactive)
  ;;(message "Caught signal %S" last-input-event)
  (pastehub-timer-handler))

(define-key special-event-map [sigusr1] 'pastehub-sigusr-handler)
(setq pastehub-timer-object 
      (run-at-time t  60.0  'pastehub-timer-handler)))

;; enable pastehub-mode
(setq pastehub-mode t)
