(defpackage :satchi.view.electron
  (:use :cl)
  (:export :make
           :icon-url
           :loading
           :make-loading
           :viewing
           :viewing-ntfs
           :viewing-incoming-notification-count
           :viewing-mention-only-p
           :make-viewing
           :ntf
           :ntf-gateway-id
           :ntf-id
           :ntf-timestamp
           :ntf-source-name
           :ntf-source-url
           :ntf-source-icon-url
           :ntf-title
           :ntf-message
           :ntf-mentioned-p
           :make-item))
(in-package :satchi.view.electron)

(defgeneric make (state))

(defstruct loading)

(defmethod make ((state satchi:loading-state))
  (make-loading))

(defgeneric icon-url (icon gw-id))
(defmethod icon-url (icon gw-id) nil)

(defstruct ntf
  gateway-id
  id
  timestamp
  source-name
  source-url
  source-icon-url
  title
  message
  mentioned-p)

(defun plist->ntf (&key gateway-id ntf)
  (let ((source (satchi.notification:notification-source ntf)))
    (make-ntf
     :gateway-id gateway-id
     :id (satchi.notification:notification-id ntf)
     :timestamp (local-time:format-timestring
                 nil
                 (satchi.notification:notification-timestamp ntf))
     :source-name (satchi.notification:source-name source)
     :source-url (satchi.notification:source-url source)
     :source-icon-url (icon-url (satchi.notification:source-icon source)
                                gateway-id)
     :title (satchi.notification:notification-title ntf)
     :message (satchi.notification:notification-message ntf)
     :mentioned-p (satchi.notification:notification-mentioned-p ntf))))

(defstruct viewing
  ntfs
  incoming-notification-count
  mention-only-p)

(defmethod make ((state satchi:viewing-state))
  (make-viewing
   :ntfs
   (satchi:viewing-state-ntfs state #'plist->ntf)
   :mention-only-p
   (satchi:viewing-state-mention-only-p state)
   :incoming-notification-count
   (satchi:viewing-state-incoming-notification-count state)))
