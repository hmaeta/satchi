(defpackage :satchi.gateway.sqlite
  (:use :cl)
  (:export :create-table))
(in-package :satchi.gateway.sqlite)

(defvar *db-path* "./satchi.sqlite")

(defun create-table ()
  (sqlite:with-open-database (db *db-path*)
    (sqlite:execute-non-query
     db (concatenate 'string
         "CREATE TABLE ntf ("
         " gateway_id char(36) NOT NULL,"
         " notification_id char(256) NOT NULL,"
         " timestamp text NOT NULL,"
         " timestamp_universal integer NOT NULL,"
         " source text NOT NULL,"
         " title text NOT NULL,"
         " message text NOT NULL,"
         " is_mentioned integer NOT NULL,"
         " PRIMARY KEY (gateway_id, notification_id)"
         ")"))
    (sqlite:execute-non-query
     db (concatenate 'string
         "CREATE TABLE ntf_state ("
         " gateway_id char(36) NOT NULL,"
         " notification_id char(256) NOT NULL,"
         " state varchar(64) NOT NULL,"
         " PRIMARY KEY (gateway_id, notification_id),"
         " FOREIGN KEY (notification_id) REFERENCES ntf(notification_id)"
         ")"))))

(defun ntf-mark (db gw-id ntf-id)
  (sqlite:execute-non-query
   db (concatenate 'string
       " UPDATE ntf_state"
       " SET"
       "   state = 'MARKED'"
       " WHERE"
       "   gateway_id = (?) AND notification_id = (?)")
   gw-id ntf-id))

(defun ntf-pooled-to-unread (db)
  (sqlite:execute-non-query
   db (concatenate 'string
       " UPDATE ntf_state"
       " SET"
       "   state = 'UNREAD'"
       " WHERE"
       "   state = 'POOLED'")))

(defun ntf-add (db gw-id ntfs)
  (dolist (ntf ntfs)
    (sqlite:execute-non-query
     db (concatenate 'string
         " INSERT OR IGNORE INTO ntf"
         "  (gateway_id, notification_id,"
         "   timestamp, timestamp_universal,"
         "   source, title, message, is_mentioned)"
         " VALUES (?, ?, ?, ?, ?, ?, ?, ?)")
     gw-id
     (satchi.notification:notification-id ntf)
     ;; timestamp
     (local-time:format-timestring
      nil
      (satchi.notification:notification-timestamp ntf))
     ;; timestamp_universal
     (local-time:timestamp-to-universal
      (satchi.notification:notification-timestamp ntf))
     ;; source
     (let ((source (satchi.notification:notification-source ntf)))
       (jsown:to-json
        (jsown:new-js
          ("name"
           (satchi.notification:source-name source))
          ("url"
           (or (satchi.notification:source-url source) :null))
          ("icon"
           (let ((icon (satchi.notification:source-icon source)))
             (etypecase icon
               (satchi.notification:icon
                (jsown:new-js
                  ("type" "icon")
                  ("url" (or (satchi.notification:icon-url icon)
                             :null))))
               (satchi.notification:private-icon
                (jsown:new-js
                  ("type" "private-icon")
                  ("url" (or (satchi.notification:private-icon-url icon)
                             :null))))
               (t :null)))))))
     ;; title
     (satchi.notification:notification-title ntf)
     ;; message
     (satchi.notification:notification-message ntf)
     ;; is_mentioned
     (if (satchi.notification:notification-mentioned-p ntf) 1 0))))

(defun ntf-add-to-pooled (db gw-id ntfs)
  (ntf-add db gw-id ntfs)
  (dolist (ntf ntfs)
    (sqlite:execute-non-query
     db (concatenate 'string
         " INSERT OR IGNORE INTO ntf_state"
         "  (gateway_id, notification_id, state)"
         " VALUES"
         "  (?, ?, 'POOLED')")
     gw-id (satchi.notification:notification-id ntf))))

(defun ntf-add-to-unread (db gw-id ntfs)
  (ntf-add db gw-id ntfs)
  (dolist (ntf ntfs)
    (sqlite:execute-non-query
     db (concatenate 'string
         " INSERT OR IGNORE INTO ntf_state"
         "  (gateway_id, notification_id, state)"
         " VALUES"
         "  (?, ?, 'UNREAD')")
     gw-id (satchi.notification:notification-id ntf))))

(defun ntf-unread-list-query (db is-mention-only keyword)
  (let ((args nil))
    (let ((q (with-output-to-string (s)
               (labels ((w (line)
                          (write-line line s)))
                 (w " SELECT")
                 (w "   ntf.gateway_id, ntf.notification_id,")
                 (w "   ntf.timestamp, ntf.source,")
                 (w "   ntf.title, ntf.message, ntf.is_mentioned")
                 (w " FROM")
                 (w "   ntf")
                 (w " INNER JOIN")
                 (w "     ntf_state")
                 (w "   ON")
                 (w "     ntf.gateway_id = ntf_state.gateway_id")
                 (w "   AND")
                 (w "     ntf.notification_id = ntf_state.notification_id")
                 (w " WHERE")
                 (w "   ntf_state.state = 'UNREAD'")
                 (when is-mention-only
                   (w " AND")
                   (w "   ntf.is_mentioned = 1"))
                 (when (and (stringp keyword)
                            (not (string= keyword "")))
                   (w " AND (")
                   (w "     ntf.message LIKE ?")
                   (push (format nil "%~A%" keyword) args)
                   (w "   OR")
                   (w "     ntf.title   LIKE ?")
                   (push (format nil "%~A%" keyword) args)
                   (w " )"))
                 (w " ORDER BY ntf.timestamp_universal DESC")
                 (w " LIMIT 100")))))
      (apply #'sqlite:execute-to-list db q args))))

(defun parse-rows (convert-fn rows)
  (loop for (gw-id ntf-id timestamp source title message mentioned-p)
            in rows
        for ntf = (satchi.notification:make-notification
                   :id ntf-id
                   :timestamp (local-time:parse-timestring timestamp)
                   :source
                   (let ((jsown (jsown:parse source)))
                     (satchi.notification:make-source
                      :name (jsown:val jsown "name")
                      :url (jsown:val jsown "url")
                      :icon
                      (let ((icon-jsown (jsown:val jsown "icon")))
                        (when icon-jsown
                          (let ((type (jsown:val icon-jsown "type")))
                            (cond ((string= type "icon")
                                   (satchi.notification:make-icon
                                    :url (jsown:val icon-jsown "url")))
                                  ((string= type "private-icon")
                                   (satchi.notification:make-private-icon
                                    :url (jsown:val icon-jsown "url")))
                                  (t (assert nil))))))))
                   :title title
                   :message message
                   :mentioned-p mentioned-p)
        collect (funcall convert-fn :gateway-id gw-id :ntf ntf)))

(defun ntf-pooled-count (db)
  (sqlite:execute-single
   db "SELECT COUNT(*) FROM ntf_state WHERE state = 'POOLED'"))

;;;

(defstruct holder db gw-id)

(defmethod satchi.notification-holder:mark ((h holder) ntf-id)
  (ntf-mark (holder-db h) (holder-gw-id h) ntf-id))

(defmethod satchi.notification-holder:add-to-unread ((h holder) ntfs)
  (ntf-add-to-unread (holder-db h) (holder-gw-id h) ntfs))

(defmethod satchi.notification-holder:add-to-pooled ((h holder) ntfs)
  (ntf-add-to-pooled (holder-db h) (holder-gw-id h) ntfs))

;;;

(defclass state (satchi.time-machine:state
                 satchi.notification-list:state
                 satchi.desktop-notification:state)
  ((gw-id
    :initarg :gw-id)
   (db
    :initarg :db)
   (offset-hash
    :initarg :offset-hash)
   (sent-ntfs-hash
    :initarg :sent-ntfs-hash)))
    
(defmethod satchi.notification-list:state-holder ((s state))
  (with-slots (gw-id db) s
    (make-holder :db db :gw-id gw-id)))

(defmethod satchi.time-machine:state-offset ((s state))
  (with-slots (gw-id offset-hash) s
    (gethash gw-id offset-hash)))

(defmethod satchi.time-machine:state-update-offset ((s state) new-offset)
  (with-slots (gw-id offset-hash) s
    (setf (gethash gw-id offset-hash) new-offset)))

(defmethod satchi.desktop-notification:update-sent ((s state) fn)
  (with-slots (gw-id sent-ntfs-hash) s
    (let ((sent-ntfs (gethash gw-id sent-ntfs-hash)))
      (setf (gethash gw-id sent-ntfs-hash) (funcall fn sent-ntfs)))))

;;;

(defstruct state-set pathname offset-hash sent-ntfs-hash)

(defmethod satchi.gateway:state-set-add-state ((state-set state-set)
                                               (gw satchi.gateway:gateway)
                                               (ntfs list))
  (sqlite:with-open-database (db (state-set-pathname state-set))
    (ntf-add-to-unread db (satchi.gateway:gateway-id gw) ntfs)))

(defmethod satchi.gateway:state-set-get-state ((state-set state-set)
                                               (gw-id t)
                                               (fn function))
  (with-slots (pathname offset-hash sent-ntfs-hash) state-set
    (sqlite:with-open-database (db pathname)
      (funcall fn (make-instance 'state
                   :gw-id gw-id
                   :db db
                   :offset-hash offset-hash
                   :sent-ntfs-hash sent-ntfs-hash)))))

(defmethod satchi.gateway:state-set-unread-list ((state-set state-set)
                                                 convert-fn
                                                 &key is-mention-only
                                                      keyword)
  (sqlite:with-open-database (db (state-set-pathname state-set))
    (parse-rows convert-fn (ntf-unread-list-query
                            db is-mention-only keyword))))

(defmethod satchi.gateway:state-set-pooled-count ((state-set state-set))
  (sqlite:with-open-database (db (state-set-pathname state-set))
    (ntf-pooled-count db)))

(defmethod satchi.gateway:state-set-pooled-flush ((state-set state-set))
  (sqlite:with-open-database (db (state-set-pathname state-set))
    (ntf-pooled-to-unread db)))

(setf satchi.gateway:*make-state-set-impl*
      (lambda ()
        (make-state-set
         :pathname *db-path*
         :offset-hash (make-hash-table :test #'equal)
         :sent-ntfs-hash (make-hash-table :test #'equal))))
