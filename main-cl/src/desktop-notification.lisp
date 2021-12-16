(defpackage :satchi.desktop-notification
  (:use :cl)
  (:export :state
           :update-sent
           :sender
           :sender-send
           :run))
(in-package :satchi.desktop-notification)

(defclass state () ())
(defgeneric update-sent (state fn))

(defgeneric sender-send (sender ntfs))

(defun run (&key client sender state)
  (let ((ntfs (satchi.notification:fetch-notifications client)))
    (update-sent state
     (lambda (sent-ntfs)
       ;; todo
       (sender-send sender ntfs)
       ;;todo
       (append ntfs sent-ntfs)))))
