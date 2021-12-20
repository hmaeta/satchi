(defpackage :satchi.notification
  (:use :cl)
  (:export :notification
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
           :make-source
           :icon
           :icon-url
           :make-icon
           :private-icon
           :private-icon-url
           :make-private-icon))
(in-package :satchi.notification)

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
