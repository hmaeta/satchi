(defpackage :satchi.client.rss
  (:use :cl)
  (:export :client
           :make-client))
(in-package :satchi.client.rss)

(defstruct client url)

(defmethod satchi.client:fetch-notifications ((client client))
  (let ((string (babel:octets-to-string
                 (drakma:http-request (client-url client)
                                      :method :get
                                      :force-binary t)
                 :encoding :utf-8)))
    (with-input-from-string (stream string)
      (let* ((channel (rss:parse-rss-stream stream))
             (items (rss:items channel)))
        (mapcar (lambda (item)
                  (satchi.notification:make-notification
                   :timestamp
                   (local-time:parse-timestring (rss:pub-date item))
                   :source
                   (satchi.notification:make-source
                    :name (rss:title channel)
                    :url (rss:link item)
                    :icon nil)
                   :title (rss:title item)
                   :message (rss:description item)
                   :mentioned-p nil
                   :id (rss:link item)))
                items)))))
