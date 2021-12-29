(defpackage :satchi.notification-list
  (:use :cl)
  (:export :state
           :state-holder
           :ref
           :mark-as-read
           :fetch-back-to-unread
           :fetch-to-pooled))
(in-package :satchi.notification-list)

(defclass state () ())
(defgeneric state-holder (state))

(defgeneric ref (state-ref fn))

(defun mark-as-read (ntf-id &key client state-ref presenter)
  (ref state-ref (lambda (state)
                   (satchi.notification-holder:mark
                    (state-holder state) ntf-id)))
  (satchi.presenter:update presenter)
  (satchi.client:mark-as-read client ntf-id))

(defun fetch-to-pooled (&key client state-ref presenter)
  (let ((ntfs (satchi.client:fetch-notifications client)))
    (ref state-ref (lambda (state)
                     (satchi.notification-holder:add-to-pooled
                      (state-holder state) ntfs))))
  (satchi.presenter:update presenter))
