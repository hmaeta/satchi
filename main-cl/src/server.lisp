(defpackage :satchi.server
  (:use :cl)
  (:export :handle-request))
(in-package :satchi.server)

(defun handle-request (service msg-string)
  (let* ((msg (jsown:parse msg-string))
         (op (jsown:val msg "op"))
         (args (jsown:val-safe msg "args")))
    (cond ((string= op "Notifications")
           (satchi:view-latest
            service))
          ((string= op "ChangeFilterKeyword")
           (satchi:change-filter-keyword
            service
            (jsown:val args "keyword")))
          ((string= op "MarkAsRead")
           (satchi:mark-as-read
            service
            (jsown:val args "gatewayId")
            (jsown:val args "notificationId")))
          ((string= op "ToggleMentioned")
           (satchi:toggle-mentioned
            service))
          ((string= op "ViewIncomingNotifications")
           (satchi:view-incoming-notifications
            service))
          (t
           (print msg)))))
