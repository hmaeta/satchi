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
           :make-state-set))
(in-package :satchi.gateway)

(defstruct gateway id client is-managed)

(defgeneric make-state-set (type))
(defgeneric state-set-add-state (state-set gw ntfs))
(defgeneric state-set-get-state (state-set gw-id fn))
(defgeneric state-set-unread-list (state-set convert-fn
                                   &key is-mention-only))
(defgeneric state-set-pooled-count (state-set))
(defgeneric state-set-pooled-flush (state-set))
