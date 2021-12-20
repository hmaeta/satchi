(defpackage :satchi.desktop-notification
  (:use :cl)
  (:export :state
           :update-sent
           :sender
           :sender-send
           :run))
(in-package :satchi.desktop-notification)

(defclass state () ())
(defgeneric update-sent (state fn))

(defun select-to-be-sent (ntfs &key sent-ntfs now)
  (let ((timestamp-select-after (local-time:timestamp- now 15 :minute)))
    (remove-if-not (lambda (ntf)
                     (and
                      (satchi.notification:notification-mentioned-p ntf)
                      ;; is-not-sent
                      (not (find (satchi.notification:notification-id ntf)
                                 sent-ntfs  ;; O(n^2)
                                 :key #'satchi.notification:notification-id
                                 ;; notification-id=
                                 :test #'string=))
                      (local-time:timestamp<
                       timestamp-select-after
                       (satchi.notification:notification-timestamp ntf))))
                   ntfs)))

(defun run (&key client send-ntfs-fn state)
  (let ((ntfs (satchi.client:fetch-notifications client)))
    (update-sent state
     (lambda (sent-ntfs)
       (let ((to-be-sent (select-to-be-sent ntfs
                                            :sent-ntfs sent-ntfs
                                            :now (local-time:now))))
         (funcall send-ntfs-fn to-be-sent)
         (append to-be-sent sent-ntfs))))))
