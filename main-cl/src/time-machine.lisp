(defpackage :satchi.time-machine
  (:use :cl)
  (:export :state
           :update-offset
           :fetch-back-to-unread))
(in-package :satchi.time-machine)

(defclass state () ())
(defgeneric update-offset (state fn))

(defun fetch-back-to-unread (&key client state)
  (update-offset state
    (lambda (offset)
      (satchi.client:fetch-notifications-from client offset))))
