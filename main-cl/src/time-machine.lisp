(defpackage :satchi.time-machine
  (:use :cl)
  (:export :state
           :state-holder
           :state-offset
           :state-update-offset
           :fetch-back-to-unread))
(in-package :satchi.time-machine)

(defclass state () ())
(defgeneric state-holder (state))
(defgeneric state-offset (state))
(defgeneric state-update-offset (state next-offset))

(defun fetch-back-to-unread (&key client state)
  (let ((result (satchi.client:fetch-notifications-from
                 client (state-offset state))))
    (when result
      (destructuring-bind (next-offset ntfs) result
        (satchi.notification-holder:add-to-unread
         (state-holder state) ntfs)
        (state-update-offset state next-offset)))))
