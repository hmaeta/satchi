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
