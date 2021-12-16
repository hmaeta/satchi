(defpackage :satchi
  (:use :cl)
  (:export :make-service
           :fetch-icon
           :fetch-to-pooled
           :handle-request))
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

(defstruct gateway-state-ref gw-id state-set)

(defmethod satchi.notification-list:ref ((ref gateway-state-ref) fn)
  (with-slots (gw-id state-set) ref
    (satchi.gateway:state-set-get-state state-set gw-id fn)))

;;;;

(defstruct loading-state)
(defstruct viewing-state gateway-state-set filter-state)

(defun viewing-state-items (state)
  (with-accessors ((filter-state viewing-state-filter-state)
                   (gw-state-set viewing-state-gateway-state-set)) state
    (satchi.gateway:state-set-unread-list
     gw-state-set #'satchi.view:make-item
     :is-mention-only
     (filter-state-is-mention-only filter-state)
     :keyword
     (filter-state-keyword filter-state))))

(defun viewing-state-gateway-state-ref (state gw-id)
  (make-gateway-state-ref
   :gw-id gw-id
   :state-set (viewing-state-gateway-state-set state)))

(defun viewing-state-gateway-state (state gw-id fn)
  (with-slots (state-set) state
    (satchi.gateway:state-set-get-state state-set gw-id fn)))

(defun viewing-state-incoming-notification-count (state)
  (with-accessors ((gw-state-set viewing-state-gateway-state-set)) state
    (satchi.gateway:state-set-pooled-count gw-state-set)))

;;;

(defstruct service state gateways send-view-fn send-ntfs-fn)

(defun gui-update (service)
  (funcall (service-send-view-fn service)
           (let ((state (service-state service)))
             (etypecase state
               (loading-state
                (satchi.view:loading))
               (viewing-state
                (satchi.view:viewing
                 :items
                 (viewing-state-items state)
                 :is-mention-only
                 (filter-state-is-mention-only
                  (viewing-state-filter-state state))
                 :incoming-notification-count
                 (viewing-state-incoming-notification-count state)))))))

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
         :state-ref (viewing-state-gateway-state-ref state gw-id)
         :renderer service)))))

(defun fetch-to-pooled (service)
  (let ((state (service-state service)))
    (when (typep state 'viewing-state)
      (dolist (gw (service-gateways service))
        (with-accessors ((id satchi.gateway:gateway-id)
                         (client satchi.gateway:gateway-client)) gw
          (satchi.notification-list:fetch-to-pooled
           :client client
           :state-ref (viewing-state-gateway-state-ref state id)
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
  (setf (service-state service)
        (make-loading-state))
  (gui-update service)
  (let ((gw-state-set (satchi.gateway:make-state-set nil)))
    (dolist (gw (service-gateways service))
      (let ((ntfs (satchi.notification:fetch-notifications
                   (satchi.gateway:gateway-client gw))))
        (satchi.gateway:state-set-add-state gw-state-set gw ntfs)))
    (setf (service-state service)
          (make-viewing-state
           :gateway-state-set gw-state-set
           :filter-state (make-instance 'filter-state)))
    (gui-update service)))

(defun fetch-icon (service gw-id icon-url)
  (let ((state (service-state service)))
    (when (typep state 'viewing-state)
      (let ((gw (find gw-id (service-gateways service)
                      :key #'satchi.gateway:gateway-id
                      ;; gw-id
                      :test #'string=)))
        (satchi.notification:fetch-icon
         (satchi.gateway:gateway-client gw) icon-url)))))

(defun handle-request (service msg-string)
  (let* ((msg (jsown:parse msg-string))
         (op (jsown:val msg "op"))
         (args (jsown:val-safe msg "args")))
    (cond ((string= op "Notifications")
           (view-latest service))
          ((string= op "ChangeFilterKeyword")
           (change-filter-keyword service
                                  (jsown:val args "keyword")))
          ((string= op "MarkAsRead")
           (mark-as-read service
                         (jsown:val args "gatewayId")
                         (jsown:val args "notificationId")))
          ((string= op "ToggleMentioned")
           (toggle-mentioned service))
          ((string= op "ViewIncomingNotifications")
           (view-incoming-notifications service))
          (t
           (print msg)))))
