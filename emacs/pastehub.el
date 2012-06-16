;; -*- coding: utf-8 -*-
;;
;; "pastehub.el" is a client program for PasteHub cloud service
;;
;;   Copyright (C) 2012 Kiyoka Nishiyama
;;

(defcustom pastehub-client "pastehub-clientPost"
  "client program name."
  :type  'string
  :group 'pastehub)

(defun posthub-post-internal ()
  (with-temp-buffer
    (insert (substring-no-properties (car kill-ring)))
    (call-process-region (point-min) (point-max)
			 pastehub-client)))

(defadvice kill-new (after pastehub-post activate)
  "Post the lastest killed text to pastehub cloud service."
  (posthub-post-internal))

(ad-activate 'kill-new)







