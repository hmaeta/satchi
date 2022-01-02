(defpackage :satchi.bin.ncurses-client
  (:use :cl)
  (:export :main))
(in-package :satchi.bin.ncurses-client)

(defstruct client ws)

(defmethod satchi.view.ncurses.ui:view-latest ((client client))
  (wsd:send
   (client-ws client)
   (jsown:to-json
    (jsown:new-js
      ("op" "Notifications")))))

(defmethod satchi.view.ncurses.ui:view-incoming-notifications
    ((client client))
  (wsd:send
   (client-ws client)
   (jsown:to-json
    (jsown:new-js
      ("op" "ViewIncomingNotifications")))))

(defun create-client (update-view-fn)
  ;; electron server
  (let ((ws (wsd:make-client "ws://localhost:8037/connect")))
    (wsd:start-connection ws)
    (wsd:on :message ws
            (lambda (msg)
              (let* ((json (jsown:parse msg))
                     (type (jsown:val json "type")))
                (cond ((string= type "UpdateView")
                       (let ((view (satchi.view.ncurses:make
                                    (jsown:val json "value"))))
                         (funcall update-view-fn view)))))))
    (make-client :ws ws)))

(defun main ()
  (let ((client-list nil))
    (unwind-protect
         (satchi.view.ncurses.ui:main
          (lambda (update-view-fn)
            (handler-case
                (let ((client (create-client update-view-fn)))
                  (push client client-list)
                  client)
              (usocket:connection-refused-error ()
                nil))))
      (dolist (client client-list)
        (ignore-errors
         (wsd:close-connection (client-ws client)))))))
