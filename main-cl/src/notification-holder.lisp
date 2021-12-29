(defpackage :satchi.notification-holder
  (:use :cl)
  (:export :holder
           :mark
           :add-to-unread
           :add-to-pooled))
(in-package :satchi.notification-holder)

(defclass holder () ())
(defgeneric mark (h ntf-id))
(defgeneric add-to-unread (h ntfs))
(defgeneric add-to-pooled (h ntfs))
