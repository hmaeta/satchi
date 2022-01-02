(defpackage :satchi.client
  (:use :cl)
  (:export :fetch-notifications
           :fetch-notifications-from
           :fetch-icon
           :mark-as-read))
(in-package :satchi.client)

(defgeneric fetch-notifications (client))
(defgeneric fetch-notifications-from (client offset))
(defmethod fetch-notifications-from (client offset))


(defgeneric fetch-icon (client icon-id))
(defmethod fetch-icon (client icon-id))

(defgeneric mark-as-read (client ntf-id))
(defmethod mark-as-read (client ntf-id))
