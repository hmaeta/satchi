(in-package :satchi.view.ncurses)

(defgeneric make (state))

(defmethod make ((state satchi:loading-state))
  (make-loading))

(defmethod make ((state satchi:viewing-state))
  (labels ((plist->ntf (&key gateway-id ntf)
             (declare (ignore gateway-id))
             (let ((source (satchi.notification:notification-source ntf)))
               (make-ntf
                :title (satchi.notification:notification-title ntf)
                :message (satchi.notification:notification-message ntf)
                :timestamp
                (local-time:format-timestring
                 nil (satchi.notification:notification-timestamp ntf))
                :source-name (satchi.notification:source-name source)
                :source-url (satchi.notification:source-url source)))))
    (make-viewing
     :ntfs (satchi:viewing-state-ntfs state #'plist->ntf))))
