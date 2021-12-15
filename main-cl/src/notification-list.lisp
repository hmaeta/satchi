(defpackage :satchi.notification-list
  (:use :cl)
  (:export :state
           :add-to-pooled
           :add-to-unread
           :mark
           :gui-update
           :mark-as-read
           :fetch-back-to-unread
           :fetch-to-pooled))
(in-package :satchi.notification-list)

(defgeneric gui-update (renderer))

(defclass state () ())
(defgeneric mark (state ntf-id))
(defgeneric add-to-pooled (state ntfs))

(defun mark-as-read (ntf-id &key client state renderer)
  (mark state ntf-id)
  (gui-update renderer)
  (satchi.notification:mark-as-read client ntf-id))

(defun fetch-to-pooled (&key client state renderer)
  (let ((ntfs (satchi.notification:fetch-notifications client)))
    (add-to-pooled state ntfs)
    (gui-update renderer)))
