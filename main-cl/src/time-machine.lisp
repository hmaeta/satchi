(defpackage :satchi.time-machine
  (:use :cl)
  (:export :state
           :update-offset
           :fetch-back-to-unread))
(in-package :satchi.time-machine)

(defclass state () ())
(defgeneric update-offset (state fn))

(defun fetch-back-to-unread (&key client state)
  (state-update-offset state
   (lambda (offset)
     (declare (ignore offset)) ;; todo
     (destructuring-bind (ntfs next-offset)
         (satchi.notification:fetch-notifications client)
       (list next-offset ntfs)))))
