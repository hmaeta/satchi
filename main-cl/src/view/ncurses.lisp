(defpackage :satchi.view.ncurses
  (:use :cl)
  (:export :loading
           :viewing
           :viewing-ntfs
           :ntf-title
           :ntf-message
           :ntf-timestamp
           :ntf-source-name
           :ntf-source-url
           :make))
(in-package :satchi.view.ncurses)

(defgeneric make (state))

(defstruct loading)

(defmethod make ((state satchi:loading-state))
  (make-loading))


(defstruct ntf title message timestamp source-name source-url)
(defstruct viewing ntfs)

(defmethod make ((state satchi:viewing-state))
  (make-viewing
   :ntfs
   (satchi.gateway:state-set-unread-list
    (satchi:viewing-state-gateway-state-set state)
    (lambda (&key gateway-id ntf)
      (declare (ignore gateway-id))
      (let ((source (satchi.notification:notification-source ntf)))
        (make-ntf
         :title (satchi.notification:notification-title ntf)
         :message (satchi.notification:notification-message ntf)
         :timestamp (local-time:format-timestring
                     nil (satchi.notification:notification-timestamp ntf))
         :source-name (satchi.notification:source-name source)
         :source-url (satchi.notification:source-url source)))))))
