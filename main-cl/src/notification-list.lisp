(defpackage :satchi.notification-list
  (:use :cl)
  (:export :ref
           :state
           :mark
           :add-to-pooled
           :gui-update
           :mark-as-read
           :fetch-back-to-unread
           :fetch-to-pooled))
(in-package :satchi.notification-list)

(defgeneric gui-update (renderer))

(defgeneric ref (state-ref fn))

(defclass state () ())
(defgeneric mark (state ntf-id))
(defgeneric add-to-pooled (state ntfs))

(defun mark-as-read (ntf-id &key client state-ref renderer)
  (ref state-ref (lambda (state)
                   (mark state ntf-id)))
  (gui-update renderer)
  (satchi.notification:mark-as-read client ntf-id))

(defun fetch-to-pooled (&key client state-ref renderer)
  (let ((ntfs (satchi.notification:fetch-notifications client)))
    (ref state-ref (lambda (state)
                     (add-to-pooled state ntfs))))
  (gui-update renderer))
