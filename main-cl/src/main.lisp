(defpackage :satchi
  (:use :cl)
  (:export :make-service
           :view-latest
           :fetch-icon
           :handle-request))
(in-package :satchi)

(defstruct state
  (gateways nil)
  (notification-list (satchi.notification-list:make-state))
  (filter (satchi.filter:make-state)))

(defclass service ()
  ((state
    :initarg :state
    :reader service-state)
   (on-update-view
    :initarg :on-update-view
    :reader service-on-update-view)))

(defun private-icon-url (icon gateway-id)
  (format nil "http://localhost:8037/icon?gatewayId=~A&iconId=~A"
          gateway-id
          (satchi.notification::private-icon-id icon)))

(defun private-icon-url-decode (url)
  (let ((alist (quri:uri-query-params (quri:uri url))))
    (list (cdr (assoc "gatewayId" alist :test #'string=))
          (cdr (assoc "iconId" alist :test #'string=)))))

(defun make-view (notification-list-state filter-state)
  (jsown:to-json
   (jsown:new-js
     ("type" "UpdateView")
     ("value"
      (etypecase notification-list-state
        (satchi.notification-list::loading-state
         (jsown:new-js
           ("stateClass" "LoadingState")))
        (satchi.notification-list::viewing-state
         (jsown:new-js
           ("stateClass" "ViewingState")
           ("stateData"
            (jsown:new-js
              ("notifications"
               (let ((gw-id-ntf-list
                      (satchi.notification-list::viewing-state-unread
                       notification-list-state)))
                 (when (satchi.filter::state-is-mention-only filter-state)
                   (setq gw-id-ntf-list
                         (remove-if-not
                          #'satchi.notification::notification-mentioned-p
                          gw-id-ntf-list
                          :key #'second)))
                 (loop for (gw-id n) in gw-id-ntf-list collect
                   (jsown:new-js
                     ("imestamp"
                      (local-time:format-timestring
                       nil
                       (satchi.notification::notification-timestamp n)))
                     ("source"
                      (let ((source (satchi.notification::notification-source
                                     n)))
                        (jsown:new-js
                          ("name"
                           (satchi.notification::source-name source))
                          ("url"
                           (satchi.notification::source-url source))
                          ("iconUrl"
                           (let ((icon (satchi.notification::source-icon
                                        source)))
                             (typecase icon
                               (satchi.notification::icon
                                (satchi.notification::icon-url icon))
                               (satchi.notification::private-icon
                                (private-icon-url icon gw-id))
                               (t :null)))))))
                     ("title"
                      (satchi.notification::notification-title n))
                     ("message"
                      (satchi.notification::notification-message n))
                     ("mentioned"
                      (if (satchi.notification::notification-mentioned-p n)
                          :t :f))
                     ("gatewayId"
                      gw-id)
                     ("id"
                      (satchi.notification::notification-id n))))))
              ("isMentionOnly"
               (if (satchi.filter::state-is-mention-only filter-state)
                   :t :f)))))))))))

(defun service-update-view (service)
  (let ((state (service-state service)))
    (funcall (service-on-update-view service)
             (make-view (state-notification-list state)
                        (state-filter state)))))

(defmethod satchi.notification-list:service-update ((service service) fn)
  (setf (state-notification-list (service-state service))
        (funcall fn (state-notification-list (service-state service))))
  (service-update-view service))

(defmethod satchi.notification-list:service-gateways ((service service))
  (state-gateways (service-state service)))

(defmethod satchi.filter:service-update ((service service) fn)
  (setf (state-filter (service-state service))
        (funcall fn (state-filter (service-state service))))
  (service-update-view service))


;;;;;;;;;;;;;;;;;;;;;;;;

(defun fetch-icon (service private-icon-url)
  (destructuring-bind (gateway-id icon-id)
      (private-icon-url-decode private-icon-url)
    (satchi.notification-list::fetch-icon service
                                          gateway-id
                                          icon-id)))

(defun make-service (on-update-view)
  (make-instance 'service
                 :state (make-state)
                 :on-update-view on-update-view))

(defun handle-request (service msg-string)
  (let* ((msg (jsown:parse msg-string))
         (op (jsown:val msg "op"))
         (args (jsown:val-safe msg "args")))
    (cond ((string= op "Notifications")
           (satchi.notification-list:view-latest
            service))
          ((string= op "MarkAsRead")
           (satchi.notification-list::mark-as-read
            service
            (jsown:val args "gatewayId")
            (jsown:val args "notificationId")))
          ((string= op "ToggleMentioned")
           (satchi.filter:toggle-mentioned
            service))
          (t
           (print msg)))))
