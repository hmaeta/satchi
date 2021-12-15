(defpackage :satchi.gateway
  (:use :cl)
  (:export :make-gateway
           :gateway-id
           :gateway-client
           :make-holder
           :holder-add-to-unread
           :state
           :state-set-get-state
           :state-set-unread-list
           :state-set-pooled-count
           :make-state-set))
(in-package :satchi.gateway)

(defstruct gateway id client is-managed)

(defclass holder () ())
(defgeneric holder-mark (h ntf-id))
(defgeneric holder-add-to-unread (h ntfs))
(defgeneric holder-add-to-pooled (h ntfs))
(defgeneric holder-unread-list (h))
(defgeneric holder-pooled-count (h))

(defun make-holder (gw)
  (if (gateway-is-managed gw)
      (make-instance 'managed-holder)
      (make-instance 'unmanaged-holder)))

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
    (let ((unread-id-hash-copy (alexandria:copy-hash-table unread-id-hash)))
      (dolist (ntf added-ntfs)
        (let ((ntf-id (satchi.notification:notification-id ntf)))
          (setf (gethash ntf-id unread-id-hash-copy) t)))
      (make-instance 'unmanaged-holder
                     :ntfs (remove-duplicates
                            (append ntfs added-ntfs)
                            :key #'satchi.notification:notification-id
                            :test #'string=)
                     :unread-id-hash unread-id-hash-copy
                     :pooled-id-hash pooled-id-hash))))

(defmethod holder-add-to-pooled ((h unmanaged-holder) added-ntfs)
  (with-slots (ntfs unread-id-hash pooled-id-hash) h
    (labels ((ntf-unread-or-pooled-p (ntf)
               (let ((ntf-id (satchi.notification:notification-id ntf)))
                 (or (gethash ntf-id unread-id-hash)
                     (gethash ntf-id pooled-id-hash))))
             (ntf-already-added-p (ntf)
               (member (satchi.notification:notification-id ntf)
                       ntfs
                       :key #'satchi.notification:notification-id
                       :test #'string=))) ;; ntf-id
      (let ((added-ntfs
             (remove-if (lambda (n)
                          (or (ntf-unread-or-pooled-p n)
                              (ntf-already-added-p n)))
                        added-ntfs))
            (pooled-id-hash-copy
             (alexandria:copy-hash-table pooled-id-hash)))
        (dolist (ntf added-ntfs)
          (let ((ntf-id (satchi.notification:notification-id ntf)))
            (setf (gethash ntf-id pooled-id-hash-copy) t)))
        (make-instance 'unmanaged-holder
                       :ntfs (append ntfs added-ntfs)
                       :unread-id-hash unread-id-hash
                       :pooled-id-hash pooled-id-hash-copy)))))

(defmethod holder-mark ((h unmanaged-holder) ntf-id)
  (with-slots (ntfs unread-id-hash pooled-id-hash) h
    (let ((unread-id-hash-copy (alexandria:copy-hash-table unread-id-hash))
          (pooled-id-hash-copy (alexandria:copy-hash-table pooled-id-hash)))
      (setf (gethash ntf-id unread-id-hash) nil)
      (setf (gethash ntf-id pooled-id-hash) nil)
    (make-instance 'unmanaged-holder
                   :ntfs ntfs
                   :unread-id-hash unread-id-hash-copy
                   :pooled-id-hash pooled-id-hash-copy))))

(defclass managed-holder (holder)
  ((unread-list
    :initform nil
    :initarg :unread-list
    :reader holder-unread-list)
   (pooled-list
    :initform nil
    :initarg :pooled-list
    :reader holder-pooled-list)))

(defmethod holder-add-to-unread ((h managed-holder) added-ntfs)
  (make-instance 'managed-holder
                 :unread-list
                 (remove-duplicates
                  ;; O(n^2)
                  (append added-ntfs (holder-unread-list h))
                  :key #'satchi.notification:notification-id
                  :test #'string=)  ;; notification-id=
                 :pooled-list
                 (holder-pooled-list h)))

(defmethod holder-pooled-count ((h managed-holder))
  (length (holder-pooled-list h)))

(defmethod holder-add-to-pooled ((h managed-holder) added-ntfs)
  (make-instance 'managed-holder
                 :unread-list
                 (holder-unread-list h)
                 :pooled-list
                 (remove-if
                  (lambda (ntf)
                    (member (satchi.notification:notification-id ntf)
                            (holder-unread-list h)
                            :key #'satchi.notification:notification-id
                            :test #'string=))  ;; notification-id=
                  (remove-duplicates
                   ;; O(n^2)
                   (append added-ntfs (holder-pooled-list h))
                   :key #'satchi.notification:notification-id
                   :test #'string=))))  ;; notification-id=

(defmethod holder-mark ((h managed-holder) ntf-id)
  (make-instance 'managed-holder
                 :unread-list
                 (remove ntf-id
                         (holder-unread-list h)
                         :key #'satchi.notification:notification-id
                         :test #'string=)  ;; notification-id=
                 :pooled-list
                 (remove ntf-id
                         (holder-pooled-list h)
                         :key #'satchi.notification:notification-id
                         :test #'string=)))

;;;

(defclass state (satchi.time-machine:state
                 satchi.notification-list:state
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
    (setf holder (holder-mark holder ntf-id))))

(defmethod satchi.notification-list:add-to-pooled ((s state) ntfs)
  (with-slots (holder) s
    (setf holder (holder-add-to-pooled holder ntfs))))

(defmethod satchi.time-machine:update-offset ((s state) fn)
  (with-slots (offset holder) s
    (destructuring-bind (next-offset ntfs) (funcall fn offset)
      (setf offset next-offset)
      (setf holder (holder-add-to-unread holder ntfs)))))

(defmethod satchi.desktop-notification:update-sent ((s state) fn)
  (with-slots (sent-ntfs) s
    (setf sent-ntfs (funcall fn sent-ntfs))))

;;;

(defstruct state-set state-hash)

(defun state-set-get-state (state-set gw-id)
  (gethash gw-id (state-set-state-hash state-set)))

(defun state-set-unread-list (state-set convert-fn &key is-mention-only)
  (let ((items nil))
    (maphash (lambda (id single)
               (with-slots (holder) single
                 (dolist (ntf (holder-unread-list holder))
                   (when (or (not is-mention-only)
                             (satchi.notification:notification-mentioned-p
                              ntf))
                     (let ((item (funcall convert-fn
                                          :gateway-id id
                                          :ntf ntf)))
                       (push item items))))))
             (state-set-state-hash state-set))
    items))

(defun state-set-pooled-count (state-set)
  (let ((state-hash (state-set-state-hash state-set)))
    (loop for single being the hash-value in state-hash
          sum (with-slots (holder) single
                (holder-pooled-count holder)))))
