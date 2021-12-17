(defpackage :satchi.view.electron
  (:use :cl)
  (:export :icon-url
           :json))
(in-package :satchi.view.electron)

(defgeneric icon-url (icon gw-id))
(defmethod icon-url (icon gw-id) nil)

(defgeneric json (state))

(defmethod json ((state satchi:loading-state))
  (jsown:new-js
    ("stateClass" "LoadingState")))


(defstruct item gateway-id ntf)

(defun json-viewing (&key items
                          is-mention-only
                          incoming-notification-count)
  (jsown:new-js
    ("stateClass" "ViewingState")
    ("stateData"
     (jsown:new-js
       ("notifications"
        (loop
          for item in items
          for gw-id = (item-gateway-id item)
          for n = (item-ntf item)
          collect
            (jsown:new-js
              ("timestamp"
               (local-time:format-timestring
                nil
                (satchi.notification:notification-timestamp n)))
              ("source"
               (let ((source (satchi.notification:notification-source n)))
                 (jsown:new-js
                   ("name" (satchi.notification:source-name source))
                   ("url" (satchi.notification:source-url source))
                   ("iconUrl" (or (icon-url
                                   (satchi.notification:source-icon source)
                                   gw-id)
                                  :null)))))
              ("title" (satchi.notification:notification-title n))
              ("message" (satchi.notification:notification-message n))
              ("mentioned" (if (satchi.notification:notification-mentioned-p
                                n)
                               :t :f))
              ("gatewayId" gw-id)
              ("id" (satchi.notification:notification-id n)))))
       ("incomingNotificationCount" incoming-notification-count)
       ("isMentionOnly" (if is-mention-only :t :f))))))

(defmethod json ((state satchi:viewing-state))
  (with-accessors
        ((filter-state satchi:viewing-state-filter-state)
         (gw-state-set satchi:viewing-state-gateway-state-set)) state
    (json-viewing
     :items (satchi.gateway:state-set-unread-list
             gw-state-set #'make-item
             :is-mention-only (satchi:filter-state-is-mention-only
                               filter-state)
             :keyword (satchi:filter-state-keyword filter-state))
     :is-mention-only (satchi:filter-state-is-mention-only filter-state)
     :incoming-notification-count
     (satchi.gateway:state-set-pooled-count gw-state-set))))
