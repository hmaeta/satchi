(defpackage :satchi.notification
  (:use :cl)
  (:export :fetch-notifications
           :fetch-icon
           :mark-as-read
           :notification
           :notification-timestamp
           :notification-source
           :notification-title
           :notification-mentioned-p
           :notification-message
           :notification-id
           :make-notification
           :source
           :source-name
           :source-icon
           :source-url
           :icon
           :icon-url
           :private-icon
           :private-icon-url
           :make-source))
(in-package :satchi.notification)

(defgeneric fetch-notifications (client))
(defgeneric fetch-icon (client icon-id))
(defgeneric mark-as-read (client ntf-id))

(defmethod fetch-icon (client icon-id))
(defmethod mark-as-read (client ntf-id))

(defstruct notification
  timestamp
  source
  title
  message
  mentioned-p
  id)

(defstruct source
  name url icon)

(defstruct icon url)

(defstruct private-icon url)
