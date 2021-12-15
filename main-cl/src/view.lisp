(defpackage :satchi.view
  (:use :cl)
  (:export :icon-url
           :loading
           :viewing
           :item
           :item-gateway-id
           :item-ntf
           :make-item))
(in-package :satchi.view)

(defgeneric icon-url (icon gw-id))
(defmethod icon-url (icon gw-id) nil)

(defun loading ()
  (jsown:new-js
    ("stateClass" "LoadingState")))

(defstruct item gateway-id ntf)

(defun viewing (&key items
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
