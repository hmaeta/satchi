(defpackage :satchi.bin.server
  (:use :cl)
  (:export :start))
(in-package :satchi.bin.server)

(defun private-icon-url (icon gateway-id)
  (format nil "http://localhost:8037/icon?gatewayId=~A&iconId=~A"
          gateway-id
          (do-urlencode:urlencode
           (satchi.notification:private-icon-url icon))))

(defmethod satchi.view.electron:icon-url
    ((icon satchi.notification:icon)
     gw-id)
  (satchi.notification:icon-url icon))

(defmethod satchi.view.electron:icon-url
    ((icon satchi.notification:private-icon)
     gw-id)
  (private-icon-url icon gw-id))

(defun private-icon-url-decode (url)
  (let ((alist (quri:uri-query-params (quri:uri url))))
    (list (cdr (assoc "gatewayId" alist :test #'string=))
          (cdr (assoc "iconId" alist :test #'string=)))))

(defvar *handler* nil)

(defvar *service* nil)

(defvar *ws-list* nil)

(defvar *worker-thread* nil)

(defun stop ()
  (when *handler*
    (clack:stop *handler*))
  (when *worker-thread*
    (ignore-errors
      (bt:destroy-thread *worker-thread*))
    (setq *worker-thread* nil)))

(defun start (&key gateways)
  (stop)
  (setq *service*
        (satchi:make-service
         :state nil
         :gateways gateways
         :send-view-fn
         (lambda (service)
           (let ((view (satchi.view.electron-json:from
                        (satchi.view.electron:make
                         (satchi:service-state service)))))
             (let ((msg (jsown:to-json
                         (jsown:new-js
                           ("type" "UpdateView")
                           ("value" view)))))
               (dolist (ws *ws-list*)
                 (websocket-driver:send ws msg)))))
         :send-ntfs-fn
         (lambda (ntfs-jsown)
           (let ((msg (jsown:to-json
                       (jsown:new-js
                         ("type" "SendDesktopNotification")
                         ("value" ntfs-jsown)))))
             (dolist (ws *ws-list*)
               (websocket-driver:send ws msg))))))
  (setq *worker-thread*
        (bt:make-thread
         (lambda ()
           (loop do (progn
                      (when *service*
                        (satchi:fetch-to-pooled *service*)
                        (satchi:fetch-back-to-unread *service*))
                      (sleep (* 10 60)))))))
  (setq *handler*
        (clack:clackup
         (let ((service *service*))
           (lambda (env)
             (let ((path-info (getf env :path-info)))
               (cond ((string= path-info "/connect")
                      (let ((ws (websocket-driver:make-server env)))
                        (push ws *ws-list*)
                        (websocket-driver:on :message ws
                         (lambda (msg)
                           (satchi.server:handle-request service msg)))
                        (websocket-driver:on :error ws
                         (lambda (error)
                           (declare (ignore error))
                           (setq *ws-list* (remove ws *ws-list*))))
                         (websocket-driver:on :close ws
                         (lambda (&key code reason)
                           (declare (ignore code reason))
                           (setq *ws-list* (remove ws *ws-list*))))
                        (lambda (responder)
                          (declare (ignore responder))
                          (websocket-driver:start-connection ws))))
                     ((string= path-info "/icon")
                      (destructuring-bind (gateway-id icon-url)
                          (private-icon-url-decode (getf env :request-uri))
                        (list 200 nil (satchi:fetch-icon
                                       service gateway-id icon-url))))
                     (t
                      (assert nil))))))
         :address "0.0.0.0"
         :debug nil
         :port 8037)))
