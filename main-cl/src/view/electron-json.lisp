(defpackage :satchi.view.electron-json
  (:use :cl)
  (:export :from))
(in-package :satchi.view.electron-json)

(defgeneric from (view))

(defmethod from ((v satchi.view.electron:loading))
  (jsown:new-js
    ("stateClass" "LoadingState")))

(defmethod from ((v satchi.view.electron:viewing))
  (jsown:new-js
    ("stateClass" "ViewingState")
    ("stateData"
     (jsown:new-js
       ("notifications"
        (loop
          for ntf in (satchi.view.electron:viewing-ntfs v)
          collect
            (jsown:new-js
              ("timestamp" (satchi.view.electron:ntf-timestamp ntf))
              ("source"
               (jsown:new-js
                 ("name" (satchi.view.electron:ntf-source-name ntf))
                 ("url" (satchi.view.electron:ntf-source-url ntf))
                 ("iconUrl"
                  (or (satchi.view.electron:ntf-source-icon-url ntf)
                      :null))))
              ("title" (satchi.view.electron:ntf-title ntf))
              ("message" (satchi.view.electron:ntf-message ntf))
              ("mentioned" (if (satchi.view.electron:ntf-mentioned-p ntf)
                               :t :f))
              ("gatewayId" (satchi.view.electron:ntf-gateway-id ntf))
              ("id" (satchi.view.electron:ntf-id ntf)))))
       ("incomingNotificationCount"
        (satchi.view.electron:viewing-incoming-notification-count v))
       ("isMentionOnly"
        (if (satchi.view.electron:viewing-mention-only-p v) :t :f))))))
