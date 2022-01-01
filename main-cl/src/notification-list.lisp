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

(defmacro with-state ((s s-ref) &body body)
  `(ref ,s-ref (lambda (,s) ,@body)))

(defun mark-as-read (ntf-id &key client state-ref presenter)
  (with-state (state state-ref)
    (let ((holder (state-holder state)))
      (satchi.notification-holder:mark holder ntf-id)))
  (satchi.presenter:update presenter)
  (satchi.client:mark-as-read client ntf-id))

(defun fetch-to-pooled (&key client state-ref presenter)
  (let ((ntfs (satchi.client:fetch-notifications client)))
    (with-state (state state-ref)
      (let ((holder (state-holder state)))
        (satchi.notification-holder:add-to-pooled holder ntfs))))
  (satchi.presenter:update presenter))
