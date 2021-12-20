(defpackage :satchi.notification-list
  (:use :cl)
  (:export :ref
           :holder
           :mark
           :add-to-pooled
           :gui-update
           :mark-as-read
           :fetch-back-to-unread
           :fetch-to-pooled))
(in-package :satchi.notification-list)

(defgeneric gui-update (renderer))

(defgeneric ref (holder-ref fn))

(defclass holder () ())
(defgeneric mark (holder ntf-id))
(defgeneric add-to-pooled (holder ntfs))

(defun mark-as-read (ntf-id &key client holder-ref renderer)
  (ref holder-ref (lambda (holder)
                    (mark holder ntf-id)))
  (gui-update renderer)
  (satchi.client:mark-as-read client ntf-id))

(defun fetch-to-pooled (&key client holder-ref renderer)
  (let ((ntfs (satchi.client:fetch-notifications client)))
    (ref holder-ref (lambda (holder)
                      (add-to-pooled holder ntfs))))
  (gui-update renderer))
