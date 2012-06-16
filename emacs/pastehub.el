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


;; memo: x-select-text must be called on X-window system...

(defun pastehub-select-text (text &optional push)
  "wrapper funciton for pastehub client"

  (with-temp-buffer
    (insert text)
    (call-process-region (point-min) (point-max)
			 pastehub-client)))

(setq interprogram-cut-function 'pastehub-select-text)


;;(pastehub-select-text "a\nb\nc\n")
