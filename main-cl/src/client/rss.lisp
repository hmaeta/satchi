(defpackage :satchi.client.rss
  (:use :cl)
  (:export :client))
(in-package :satchi.client.rss)

(defstruct client url)

(defmethod satchi.notification:fetch-notifications ((client client))
  (let* ((stream (dex:get (client-url client) :want-stream t))
         (channel (rss:parse-rss-stream stream))
         (items (rss:items channel)))
    (mapcar (lambda (item)
              (satchi.notification:make-notification
               :timestamp
               (local-time:parse-timestring (rss:pub-date item))
               :source
               (satchi.notification::make-source
                :name (rss:title channel)
                :url (rss:link item)
                :icon nil)
               :title (rss:title item)
               :message (rss:description item)
               :mentioned-p nil
               :id (rss:link item)))
            items)))
