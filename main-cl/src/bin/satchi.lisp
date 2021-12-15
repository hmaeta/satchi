(defpackage :satchi.bin.satchi
  (:use :cl)
  (:export :main))
(in-package :satchi.bin.satchi)

(defun start-server (handle-fn)
  (loop for line = (read-line *standard-input* nil nil)
        while line do (funcall handle-fn line)))

(defmethod satchi.view:icon-url ((icon satchi.notification:icon)
                                 gw-id)
  (satchi.notification:icon-url icon))

(defmethod satchi.view:icon-url ((icon satchi.notification:private-icon)
                                 gw-id)
  (satchi.notification:private-icon-url icon))

(defun main ()
  (let ((service
         (satchi:make-service
          :gateways
          (list
           (satchi:make-gateway
            :id "1"
            :client (satchi.client.rss::make-client
                     :url
                     "https://news.yahoo.co.jp/rss/topics/top-picks.xml")))
          :send-view-fn
          (lambda (v-jsown)
            (let ((v (jsown:to-json
                      (jsown:new-js
                        ("type" "UpdateView")
                        ("value" v-jsown)))))
              (write-line v *standard-output*)
              (force-output *standard-output*)))
          :send-ntfs-fn
          (lambda (ntfs-jsown)
            (let ((ntfs (jsown:to-json
                         (jsown:new-js
                           ("type" "SendDesktopNotification")
                           ("value" ntfs-jsown)))))
              (write-line ntfs *standard-output*)
              (force-output *standard-output*))))))
    (start-server (lambda (line)
                    (satchi:handle-request service line)))))
