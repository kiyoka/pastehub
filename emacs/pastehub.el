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

(defvar pastehub-lastest-key ""       "lastest synced database key.")


(defun posthub-post-internal ()
  (when (car kill-ring)
    (with-temp-buffer
      (insert (substring-no-properties (car kill-ring)))
      (call-process-region (point-min) (point-max)
			   pastehub-client-post))))

(defadvice kill-new (after pastehub-post activate)
  "Post the lastest killed text to pastehub cloud service."
  (posthub-post-internal))

(ad-activate 'kill-new)


(defun pastehub-timer-handler ()
  "polling process handler for pastehub service."
  (let ((outbuf (get-buffer-create "*pastehub dump*")))
    (with-current-buffer (buffer-name outbuf)
      (delete-region (point-min) (point-max)))
    (with-temp-buffer
      (call-process  pastehub-client-dump
		     nil ;; infile
		     outbuf
		     nil ;; display
		     "top"))
    (with-current-buffer (buffer-name outbuf)
      (let ((str (buffer-substring-no-properties (point-min) (point-max))))
	(setq kill-ring (list str))))))

(pastehub-timer-handler)

(run-at-time t 1.0 'pastehub-timer-handler)







