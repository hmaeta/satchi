(defpackage :satchi.ws
  (:use :cl)
  (:export :start))
(in-package :satchi.ws)

(defvar *handler* nil)

(defvar *service* nil)

(defun stop ()
  (when *handler*
    (clack:stop *handler*)))

(defun start ()
  (stop)
  (setq *handler*
        (clack:clackup
         (let* ((ws-list nil)
                (service (satchi:make-service
                          (lambda (v)
                            (dolist (ws ws-list)
                              (websocket-driver:send ws v))))))
           (setq *service* service)
           (lambda (env)
             (let ((path-info (getf env :path-info)))
               (cond ((string= path-info "/connect")
                      (let ((ws (websocket-driver:make-server env)))
                        (push ws ws-list)
                        (websocket-driver:on
                         :message ws (lambda (msg)
                                       (satchi:handle-request service
                                                              msg)))
                        (lambda (responder)
                          (declare (ignore responder))
                          (websocket-driver:start-connection ws))))
                     ((string= path-info "/icon")
                      (list 200 nil (satchi:fetch-icon
                                     service
                                     (getf env :request-uri))))
                     (t
                      (assert nil))))))
         :address "0.0.0.0"
         :debug nil
         :port 8037)))

#+nil
(setf (satchi::state-gateways
       (SATCHI::SERVICE-STATE SATCHI.WS::*SERVICE*))
      (list (satchi.notification-list::make-gateway
             :id "1"
             :client
             (satchi.client.rss::make-client
              :url "https://news.yahoo.co.jp/rss/topics/top-picks.xml"))))
