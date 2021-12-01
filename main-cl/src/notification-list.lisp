(defpackage :satchi.notification-list
  (:use :cl)
  (:export :state
           :make-state
           :loading-state
           :make-loading-state
           :viewing-state
           :viewing-state-ntfs
           :make-viewing-state

           :service-update
           :service-gateways
           :view-latest))
(in-package :satchi.notification-list)

(defclass holder () ())
(defgeneric holder-unread-list (h))
(defgeneric holder-add-unread (h ntfs))
(defgeneric holder-mark (h ntf-id))

(defclass gw-holder (holder)
  ((ntfs
    :initarg :ntfs
    :initform nil)
   (read-id-hash
    :initarg :read-id-hash
    :initform (make-hash-table :test #'equal)))) ;; notification-id=

(defmethod holder-unread-list ((h gw-holder))
  (with-slots (ntfs read-id-hash) h
    (labels ((ntf-read-p (ntf)
               (let ((ntf-id (satchi.notification::notification-id ntf)))
                 (gethash ntf-id read-id-hash))))
      (remove-if #'ntf-read-p ntfs))))

(defmethod holder-add-unread ((h gw-holder) ntfs)
  (make-instance
   'gw-holder
   :ntfs (append ntfs (slot-value h 'ntfs))
   :read-id-hash (slot-value h 'read-id-hash)))

(defmethod holder-mark ((h gw-holder) ntf-id)
  (let ((hash (alexandria:copy-hash-table
               (slot-value h 'read-id-hash))))
    (setf (gethash ntf-id hash) t)
    (make-instance
     'gw-holder
     :ntfs (slot-value h 'ntfs)
     :read-id-hash hash)))

(defclass managed-gw-holder (holder)
  ((unread-list
    :initform nil
    :initarg :unread-list
    :reader holder-unread-list)))

(defmethod holder-add-unread ((h managed-gw-holder) ntfs)
  (make-instance
   'managed-gw-holder
   :unread-list (append ntfs (holder-unread-list h))))

(defmethod holder-mark ((h managed-gw-holder) ntf-id)
  (make-instance
   'managed-gw-holder
   :unread-list (remove ntf-id
                        (holder-unread-list h)
                        :key #'satchi.notification::notification-id
                        :test #'string=))) ;; notification-id=

(defstruct gateway id client is-managed)

(defun gateway-holder-new (gw)
  (if (gateway-is-managed gw)
      (make-instance 'managed-gw-holder)
      (make-instance 'gw-holder)))

(defstruct holder-set alist)

(defun holder-set-new ()
  (make-holder-set
   :alist nil))

(defun holder-set-update (set gw-id fn)
  (let ((alist (holder-set-alist set))
        (gw-id= #'string=))
    (make-holder-set
     :alist (if (member gw-id alist :key #'car :test gw-id=)
                (loop for (id . holder) in alist
                      collect (cons id (if (string= id gw-id)
                                           (funcall fn holder)
                                           holder)))
                (cons (cons gw-id (funcall fn nil))
                      alist)))))

(defun holder-set-unread (set)
  (let ((alist (holder-set-alist set)))
    (loop for (gw-id . holder) in alist
       nconc (mapcar (lambda (ntf)
                       (list gw-id ntf))
                     (holder-unread-list holder)))))

(defstruct state)

(defstruct loading-state holder-set)

(defstruct viewing-state holder-set)

(defun viewing-state-unread (state)
  (holder-set-unread (viewing-state-holder-set state)))

(defgeneric service-update (service fn))

(defgeneric service-gateways (service))

(defun view-latest (service)
  (let ((gateways (service-gateways service)))
    (if (null gateways)
        (service-update service
         (lambda (s)
           (declare (ignore s))
           (make-viewing-state
            :holder-set (holder-set-new))))
        (progn
          (service-update service
           (lambda (s)
             (etypecase s
               (loading-state
                s)
               (viewing-state
                (make-loading-state
                 :holder-set (viewing-state-holder-set s)))
               (state
                (make-loading-state
                 :holder-set (holder-set-new))))))
          (dolist (gw gateways)
            (let ((ntfs (satchi.notification:fetch-notifications
                         (gateway-client gw))))
              (service-update service
               (lambda (s)
                 (let ((holder-set
                        (etypecase s
                          (loading-state
                           (loading-state-holder-set s))
                          (viewing-state
                           (viewing-state-holder-set s)))))
                   (make-viewing-state
                    :holder-set
                    (holder-set-update holder-set (gateway-id gw)
                     (lambda (h)
                       (if h
                           (holder-add-unread h ntfs)
                           (gateway-holder-new gw))))))))))))))

(defun mark-as-read (service gw-id ntf-id)
  (service-update service
   (lambda (s)
     (labels ((holder-set-mark (holder-set)
                (holder-set-update holder-set gw-id
                 (lambda (h)
                   (assert h)
                   (holder-mark h ntf-id)))))
       (etypecase s
         (loading-state
          (make-loading-state
           :holder-set (holder-set-mark (loading-state-holder-set s))))
         (viewing-state
          (make-viewing-state
           :holder-set (holder-set-mark (viewing-state-holder-set s))))))))
  ;; todo: mark more
  )

(defun fetch-icon (service gw-id icon-id)
  (let ((gw (find gw-id (service-gateways service)
                  :key #'gateway-id
                  :test #'string=))) ;; gateway-id=
    (when gw
      (satchi.notification::fetch-icon (gateway-client gw) icon-id))))
