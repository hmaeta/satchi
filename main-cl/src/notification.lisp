(defpackage :satchi.notification
  (:use :cl)
  (:export :fetch-notifications
           :notification
           :make-notification
           :source
           :make-source))
(in-package :satchi.notification)

(defgeneric fetch-notifications (client))

(defgeneric fetch-icon (client icon-id))

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

(defstruct private-icon id)
