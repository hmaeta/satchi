(defpackage :satchi
  (:use :cl)
  (:export :service
           :service-state
           :make-service
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

(defstruct loading-state)
(defstruct viewing-state gateway-state-set filter-state)

(defstruct service state gateways send-view-fn send-ntfs-fn)

(defun service-presenter (service)
  (labels ((update-view ()
             (funcall (service-send-view-fn service) service)))
    (make-instance 'satchi.presenter:presenter
                   :update-fn #'update-view)))

(defun toggle-mentioned (service)
  (let ((state (service-state service)))
    (when (typep state 'viewing-state)
      (satchi.filter:toggle-mentioned
       :state (viewing-state-filter-state state)
       :presenter (service-presenter service)))))

(defun change-filter-keyword (service keyword)
  (let ((state (service-state service)))
    (when (typep state 'viewing-state)
      (satchi.filter:change-keyword keyword
       :state (viewing-state-filter-state state)
       :presenter (service-presenter service)))))

(defun mark-as-read (service gw-id ntf-id)
  (let ((state (service-state service)))
    (when (typep state 'viewing-state)
      (let ((state-set (viewing-state-gateway-state-set state)))
        (let ((gw (find gw-id (service-gateways service)
                        :key #'satchi.gateway:gateway-id
                        :test #'string=))) ;; gw-id
          (satchi.notification-list:mark-as-read ntf-id
           :client (satchi.gateway:gateway-client gw)
           :state-ref (satchi.gateway:state-set-state-ref
                       state-set
                       (satchi.gateway:gateway-id gw))
           :presenter (service-presenter service)))))))

(defun fetch-to-pooled (service)
  (let ((state (service-state service)))
    (when (typep state 'viewing-state)
      (let ((state-set (viewing-state-gateway-state-set state)))
        (dolist (gw (service-gateways service))
          (satchi.notification-list:fetch-to-pooled
           :client (satchi.gateway:gateway-client gw)
           :state-ref (satchi.gateway:state-set-state-ref
                       state-set
                       (satchi.gateway:gateway-id gw))
           :presenter (service-presenter service)))))))

(defun fetch-back-to-unread (service)
  (let ((state (service-state service)))
    (when (typep state 'viewing-state)
      (let ((state-set (viewing-state-gateway-state-set state)))
        (dolist (gw (service-gateways service))
          (with-accessors ((id satchi.gateway:gateway-id)
                           (client satchi.gateway:gateway-client)) gw
          (satchi.gateway:state-set-get-state state-set id
           (lambda (gw-state)
             (satchi.time-machine:fetch-back-to-unread
              :client client
              :state gw-state)))))))))

(defun view-incoming-notifications (service)
  (let ((state (service-state service)))
    (when (typep state 'viewing-state)
      (let ((state-set (viewing-state-gateway-state-set state)))
        (satchi.gateway:state-set-pooled-flush state-set))
      (satchi.presenter:update (service-presenter service)))))

(defun send-desktop-notification (service)
  (let ((state (service-state service)))
    (when (and (typep state 'viewing-state)
               (service-send-ntfs-fn service))
      (let ((state-set (viewing-state-gateway-state-set state)))
        (dolist (gw (service-gateways service))
          (with-accessors ((id satchi.gateway:gateway-id)
                           (client satchi.gateway:gateway-client)) gw
            (satchi.gateway:state-set-get-state state-set id
             (lambda (gw-state)
               (satchi.desktop-notification:run
                :client client
                :state gw-state
                :send-ntfs-fn (service-send-ntfs-fn service))))))))))

(defun view-latest (service)
  (with-slots (state) service
    (if state
        (satchi.presenter:update (service-presenter service))
        (satchi.view-latest:run (service-gateways service)
         :update-loading-fn
         (lambda ()
           (setf state (make-loading-state))
           (satchi.presenter:update (service-presenter service)))
         :update-viewing-fn
         (lambda (gw-state-set)
           (setf state (make-viewing-state
                        :gateway-state-set gw-state-set
                        :filter-state (make-instance 'filter-state)))
           (satchi.presenter:update (service-presenter service)))))))

(defun fetch-icon (service gw-id icon-url)
  (let ((state (service-state service)))
    (when (typep state 'viewing-state)
      (let ((gw (find gw-id (service-gateways service)
                      :key #'satchi.gateway:gateway-id
                      ;; gw-id
                      :test #'string=)))
        (satchi.client:fetch-icon (satchi.gateway:gateway-client gw)
                                  icon-url)))))
