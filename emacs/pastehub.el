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

(defvar pastehub-latest-date ""       "latest synced date.")


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

;; like scheme's `take'
(defun pastehub-take (lst n)
  (reverse (last (reverse lst)
		 n)))

(defun pastehub-sync-kill-ring ()
  "sync kill-ring"
  (message "syncing kill-ring...")
  (let* ((keys-string (pastehub-call-process pastehub-client-dump "list" ""))
	 (keys
	  (pastehub-take (split-string 
			  keys-string
			  "\n")
			 pastehub-sync-items)))
    (setq kill-ring
	  (mapcar
	   (lambda (key)
	     (pastehub-call-process pastehub-client-dump "get" key))
	   keys))
    (setq kill-ring-yank-pointer kill-ring))
  (message nil))


(defun pastehub-timer-handler ()
  "polling process handler for pastehub service."
  (let ((latest-date
	 (pastehub-call-process pastehub-client-dump "latestdate" "")))
    (if (not (string-equal pastehub-latest-date latest-date))
	(progn
	  (setq pastehub-latest-date latest-date)
	  (pastehub-sync-kill-ring)))))
	    
(pastehub-timer-handler)

(run-at-time t 1.0 'pastehub-timer-handler)

