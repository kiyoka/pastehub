;; -*- coding: utf-8 -*-
;;
;; "synchrobase.el" is a client program for SynchroBase server
;;
;;   Copyright (C) 2012 Kiyoka Nishiyama
;;


;; memo: x-select-text must be called on X-window system...

(defun sb-select-text (text &optional push)
  "wrapper funciton for syncro-base client"

  (let ((tmp-outbuf)
	(with-temp-buffer))
    (with-temp-buffer
      (insert text)
      (call-process-region (point-min) (point-max)
			   "sb-clientPost"))))

(setq interprogram-cut-function 'sb-select-text)



;;(sx-select-text "a\nb\nc\n")

