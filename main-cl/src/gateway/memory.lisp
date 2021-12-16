(defpackage :satchi.gateway.memory
  (:use :cl))
(in-package :satchi.gateway.memory)

(defclass holder () ())
(defgeneric holder-mark (h ntf-id))
(defgeneric holder-add-to-unread (h ntfs))
(defgeneric holder-add-to-pooled (h ntfs))
(defgeneric holder-unread-list (h))
(defgeneric holder-pooled-count (h))
(defgeneric holder-pooled-flush (h))

(defclass unmanaged-holder (holder)
  ((ntfs
    :initarg :ntfs
    :initform nil)
   (unread-id-hash
    :initarg :unread-id-hash
    ;; notification-id=
    :initform (make-hash-table :test #'equal))
   (pooled-id-hash
    :initarg :pooled-id-hash
    ;; notification-id=
    :initform (make-hash-table :test #'equal))))

(defmethod holder-unread-list ((h unmanaged-holder))
  (with-slots (ntfs unread-id-hash) h
    (labels ((ntf-unread-p (ntf)
               (let ((ntf-id (satchi.notification:notification-id ntf)))
                 (gethash ntf-id unread-id-hash))))
      (remove-if-not #'ntf-unread-p ntfs))))

(defmethod holder-pooled-count ((h unmanaged-holder))
  (with-slots (pooled-id-hash) h
    (hash-table-count pooled-id-hash)))

(defmethod holder-add-to-unread ((h unmanaged-holder) added-ntfs)
  (with-slots (ntfs unread-id-hash pooled-id-hash) h
    (setf ntfs (remove-duplicates (append ntfs added-ntfs)
                :key #'satchi.notification:notification-id
                :test #'string=))
    (dolist (ntf added-ntfs)
      (let ((ntf-id (satchi.notification:notification-id ntf)))
        (setf (gethash ntf-id unread-id-hash) t)))))

(defmethod holder-add-to-pooled ((h unmanaged-holder) added-ntfs)
  (with-slots (ntfs unread-id-hash pooled-id-hash) h
    (labels ((ntf-unread-or-pooled-or-added-p (ntf)
               (let ((ntf-id (satchi.notification:notification-id ntf)))
                 (or (gethash ntf-id unread-id-hash)
                     (gethash ntf-id pooled-id-hash)
                     (member ntf-id ntfs
                             :key #'satchi.notification:notification-id
                             ;; ntf-id
                             :test #'string=)))))
      (let ((added-ntfs (remove-if #'ntf-unread-or-pooled-or-added-p
                                   added-ntfs)))
        (setf ntfs (append ntfs added-ntfs))
        (dolist (ntf added-ntfs)
          (let ((ntf-id (satchi.notification:notification-id ntf)))
            (setf (gethash ntf-id pooled-id-hash) t)))))))

(defmethod holder-mark ((h unmanaged-holder) ntf-id)
  (with-slots (unread-id-hash pooled-id-hash) h
    (remhash ntf-id unread-id-hash)
    (remhash ntf-id pooled-id-hash)))

(defmethod holder-pooled-flush ((h unmanaged-holder))
  (with-slots (unread-id-hash pooled-id-hash) h
    (loop for id being the hash-key of pooled-id-hash
          do (setf (gethash id unread-id-hash) t))
    (clrhash pooled-id-hash)))

(defclass managed-holder (holder)
  ((unread-list
    :initform nil
    :initarg :unread-list)
   (pooled-list
    :initform nil
    :initarg :pooled-list)))

(defmethod holder-unread-list ((h unmanaged-holder))
  (slot-value h 'unread-list))

(defmethod holder-pooled-count ((h managed-holder))
  (length (slot-value h 'pooled-list)))

(defmethod holder-add-to-unread ((h managed-holder) added-ntfs)
  (with-slots (unread-list pooled-list) h
    ;; O(n^2)
    (setf unread-list
          (remove-duplicates (append added-ntfs unread-list)
           :key #'satchi.notification:notification-id
           ;; notification-id=
           :test #'string=))
    (setf pooled-list
          (remove-if (lambda (ntf)
                       (member ntf unread-list
                        :key #'satchi.notification:notification-id
                        ;; notification-id=
                        :test #'string=))
                     pooled-list))))

(defmethod holder-add-to-pooled ((h managed-holder) added-ntfs)
  (with-slots (unread-list pooled-list) h
    ;; O(n^2)
    (setf pooled-list
          (remove-if (lambda (ntf)
                       (member ntf unread-list
                        :key #'satchi.notification:notification-id
                        ;; notification-id=
                        :test #'string=))
                     (remove-duplicates (append added-ntfs pooled-list)
                      :key #'satchi.notification:notification-id
                      ;; notification-id=
                      :test #'string=)))))

(defmethod holder-mark ((h managed-holder) ntf-id)
  (with-slots (unread-list pooled-list) h
    (setf unread-list (remove ntf-id unread-list
                       :key #'satchi.notification:notification-id
                       ;; notification-id=
                       :test #'string=))
    (setf pooled-list (remove ntf-id pooled-list
                       :key #'satchi.notification:notification-id
                       :test #'string=))))

(defmethod holder-pooled-flush ((h managed-holder))
  (with-slots (unread-list pooled-list) h
    (setf unread-list (append unread-list pooled-list))
    (setf pooled-list nil)))
  
;;;

(defclass state (satchi.time-machine:state
                 satchi.notification-list:holder
                 satchi.desktop-notification:state)
  ((holder
    :type holder
    :initarg :holder)
   (sent-ntfs
    :initarg :sent-ntfs)
   (offset
    :initarg :offset)))

(defmethod satchi.notification-list:mark ((s state) ntf-id)
  (with-slots (holder) s
    (holder-mark holder ntf-id)))

(defmethod satchi.notification-list:add-to-pooled ((s state) ntfs)
  (with-slots (holder) s
    (holder-add-to-pooled holder ntfs)))

(defmethod satchi.time-machine:update-offset ((s state) fn)
  (with-slots (offset holder) s
    (destructuring-bind (next-offset ntfs) (funcall fn offset)
      (setf offset next-offset)
      (holder-add-to-unread holder ntfs))))

(defmethod satchi.desktop-notification:update-sent ((s state) fn)
  (with-slots (sent-ntfs) s
    (setf sent-ntfs (funcall fn sent-ntfs))))

(defstruct state-set state-hash)

(setf satchi.gateway:*make-state-set-impl*
      (lambda ()
        (make-state-set
         :state-hash (make-hash-table :test #'equal))))

(defun make-holder (gw)
  (if (satchi.gateway:gateway-is-managed gw)
      (make-instance 'managed-holder)
      (make-instance 'unmanaged-holder)))

(defmethod satchi.gateway:state-set-add-state ((state-set state-set)
                                               (gw satchi.gateway:gateway)
                                               (ntfs list))
  (let ((holder (make-holder gw)))
    (holder-add-to-unread holder ntfs)
    (let ((gw-id (satchi.gateway:gateway-id gw))
          (state (make-instance 'state
                                :holder holder
                                :sent-ntfs nil
                                :offset "")))
      (setf (gethash gw-id (state-set-state-hash state-set)) state))))

(defmethod satchi.gateway:state-set-get-state ((state-set state-set)
                                               (gw-id t)
                                               (fn function))
  (funcall fn (gethash gw-id (state-set-state-hash state-set))))

(defmethod satchi.gateway:state-set-unread-list ((state-set state-set)
                                                 convert-fn
                                                 &key is-mention-only keyword)
  (declare (ignore keyword))
  (let ((items nil))
    (maphash (lambda (id state)
               (with-slots (holder) state
                 (dolist (ntf (holder-unread-list holder))
                   (push (list id ntf) items))))
             (state-set-state-hash state-set))
    (when is-mention-only
      (setq items
            (remove-if-not
             #'satchi.notification:notification-mentioned-p items
             :key #'second)))
    (setq items
          (sort items #'>
                :key (lambda (item)
                       (local-time:timestamp-to-universal
                        (satchi.notification:notification-timestamp
                         (second item))))))
    (mapcar (lambda (item)
              (funcall convert-fn
                       :gateway-id (first item)
                       :ntf (second item)))
            items)))

(defmethod satchi.gateway:state-set-pooled-count ((state-set state-set))
  (let ((state-hash (state-set-state-hash state-set)))
    (loop for state being the hash-value in state-hash
          sum (with-slots (holder) state
                (holder-pooled-count holder)))))

(defmethod satchi.gateway:state-set-pooled-flush ((state-set state-set))
  (let ((state-hash (state-set-state-hash state-set)))
    (loop for state being the hash-value in state-hash
          do (with-slots (holder) state
               (holder-pooled-flush holder)))))
