(defpackage :satchi.gateway
  (:use :cl)
  (:export :gateway
           :gateway-id
           :gateway-client
           :gateway-is-managed
           :make-gateway
           :state-set-add-state
           :state-set-get-state
           :state-set-unread-list
           :state-set-pooled-count
           :state-set-pooled-flush
           :state-set-state-ref
           :make-state-set
           :*make-state-set-impl*))
(in-package :satchi.gateway)

(defvar *make-state-set-impl* nil)

(defstruct gateway id client is-managed)

(defun make-state-set ()
  (funcall *make-state-set-impl*))

(defgeneric state-set-add-state (state-set gw ntfs))
(defgeneric state-set-get-state (state-set gw-id fn))
(defgeneric state-set-unread-list (state-set convert-fn &key))
(defgeneric state-set-pooled-count (state-set))
(defgeneric state-set-pooled-flush (state-set))

(defstruct state-ref get-state)

(defun state-set-state-ref (state-set gw-id)
  (labels ((get-state (fn)
             (state-set-get-state state-set gw-id fn)))
    (make-state-ref :get-state #'get-state)))

(defmethod satchi.notification-list:ref ((state-ref state-ref) fn)
  (funcall (state-ref-get-state state-ref) fn))
