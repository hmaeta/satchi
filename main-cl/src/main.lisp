(defpackage :satchi
  (:use :cl)
  (:export :make-service
           :service-state
           :loading-state
           :viewing-state
           :viewing-state-filter-state
           :viewing-state-gateway-state-set
           :filter-state-is-mention-only
           :filter-state-keyword
           :fetch-icon
           :view-latest
           :change-filter-keyword
           :mark-as-read
           :toggle-mentioned
           :view-incoming-notifications
           :fetch-to-pooled))
(in-package :satchi)

(defclass filter-state (satchi.filter:state)
  ((is-mention-only
    :initform nil
    :reader filter-state-is-mention-only)
   (keyword
    :initform ""
    :reader filter-state-keyword)))

(defmethod satchi.filter:update-mentioned ((s filter-state) fn)
  (with-slots (is-mention-only) s
    (setf is-mention-only (funcall fn is-mention-only))))

(defmethod satchi.filter:update-keyword ((s filter-state) str)
  (with-slots (keyword) s
    (setf keyword str)))

;;;;

(defstruct gateway-holder-ref gw-id state-set)

(defmethod satchi.notification-list:ref ((ref gateway-holder-ref) fn)
  (with-slots (gw-id state-set) ref
    (satchi.gateway:state-set-get-state state-set gw-id fn)))

;;;;

(defstruct loading-state)
(defstruct viewing-state gateway-state-set filter-state)

(defun viewing-state-gateway-holder-ref (state gw-id)
  (make-gateway-holder-ref
   :gw-id gw-id
   :state-set (viewing-state-gateway-state-set state)))

(defun viewing-state-gateway-state (state gw-id fn)
  (with-slots (state-set) state
    (satchi.gateway:state-set-get-state state-set gw-id fn)))

;;;

(defstruct service state gateways send-view-fn send-ntfs-fn)

(defun gui-update (service)
  (funcall (service-send-view-fn service) service))

(defmethod satchi.notification-list:gui-update ((s service))
  (gui-update s))

(defmethod satchi.filter:gui-update ((s service))
  (gui-update s))

(defmethod satchi.desktop-notification:sender-send ((s service) ntfs)
  (funcall (service-send-ntfs-fn s) ntfs))


(defun toggle-mentioned (service)
  (let ((state (service-state service)))
    (when (typep state 'viewing-state)
      (satchi.filter:toggle-mentioned
       :state (viewing-state-filter-state state)
       :renderer service))))

(defun change-filter-keyword (service keyword)
  (let ((state (service-state service)))
    (when (typep state 'viewing-state)
      (satchi.filter:change-keyword keyword
       :state (viewing-state-filter-state state)
       :renderer service))))

(defun mark-as-read (service gw-id ntf-id)
  (let ((state (service-state service)))
    (when (typep state 'viewing-state)
      (let ((gw (find gw-id (service-gateways service)
                      :key #'satchi.gateway:gateway-id
                      :test #'string=))) ;; gw-id
        (satchi.notification-list:mark-as-read ntf-id
         :client (satchi.gateway:gateway-client gw)
         :holder-ref (viewing-state-gateway-holder-ref state gw-id)
         :renderer service)))))

(defun fetch-to-pooled (service)
  (let ((state (service-state service)))
    (when (typep state 'viewing-state)
      (dolist (gw (service-gateways service))
        (with-accessors ((id satchi.gateway:gateway-id)
                         (client satchi.gateway:gateway-client)) gw
          (satchi.notification-list:fetch-to-pooled
           :client client
           :holder-ref (viewing-state-gateway-holder-ref state id)
           :renderer service))))))

(defun fetch-back-to-unread (service)
  (let ((state (service-state service)))
    (when (typep state 'viewing-state)
      (dolist (gw (service-gateways service))
        (with-accessors ((id satchi.gateway:gateway-id)
                         (client satchi.gateway:gateway-client)) gw
          (viewing-state-gateway-state id state
           (lambda (gw-state)
             (satchi.time-machine:fetch-back-to-unread
              :client client
              :state gw-state))))))))

(defun view-incoming-notifications (service)
  (let ((state (service-state service)))
    (when (typep state 'viewing-state)
      (let ((state-set (viewing-state-gateway-state-set state)))
        (satchi.gateway:state-set-pooled-flush state-set))
      (gui-update service))))

(defun send-desktop-notification (service)
  (let ((state (service-state service)))
    (when (typep state 'viewing-state)
      (dolist (gw (service-gateways service))
        (with-accessors ((id satchi.gateway:gateway-id)
                         (client satchi.gateway:gateway-client)) gw
          (viewing-state-gateway-state id state
           (lambda (gw-state)
             (satchi.desktop-notification:run
              :client client
              :sender service
              :state gw-state))))))))

(defun view-latest (service)
  (with-slots (state) service
    (satchi.view-latest:run (service-gateways service)
     :update-loading-fn
     (lambda ()
       (setf state (make-loading-state))
       (gui-update service))
     :update-viewing-fn
     (lambda (gw-state-set)
       (setf state (make-viewing-state
                    :gateway-state-set gw-state-set
                    :filter-state (make-instance 'filter-state)))
       (gui-update service)))))

(defun fetch-icon (service gw-id icon-url)
  (let ((state (service-state service)))
    (when (typep state 'viewing-state)
      (let ((gw (find gw-id (service-gateways service)
                      :key #'satchi.gateway:gateway-id
                      ;; gw-id
                      :test #'string=)))
        (satchi.notification:fetch-icon
         (satchi.gateway:gateway-client gw) icon-url)))))
