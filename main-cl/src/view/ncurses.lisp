(defpackage :satchi.view.ncurses
  (:use :cl)
  (:export :paint))
(in-package :satchi.view.ncurses)

(defgeneric paint (state wnd))

(defmethod paint ((state satchi:loading-state) wnd)
  (multiple-value-bind (width height)
      (charms:window-dimensions wnd)
    (let* ((message "Loading...")
           (length/2 (floor (length message) 2)))
      (charms:write-string-at-point wnd
                                    message
                                    (- (floor width 2) length/2)
                                    (floor height 2)))))

(defmethod paint ((state satchi:viewing-state) wnd)
  (labels ((w (str x y)
             (charms:write-string-at-point wnd str x y)))
    (with-accessors
          ((gw-state-set satchi:viewing-state-gateway-state-set)) state
      (let ((ntfs (satchi.gateway:state-set-unread-list
                   gw-state-set
                   (lambda (&key gateway-id ntf)
                     (declare (ignore gateway-id))
                     ntf)))
            (y -1))
        (dolist (ntf ntfs)
          (w (satchi.notification:notification-title ntf)
             0 (incf y))
          (w (satchi.notification:notification-message ntf)
             2 (incf y))
          (incf y)
          (w (local-time:format-timestring
              nil (satchi.notification:notification-timestamp ntf))
             2 (incf y))
          (let ((source (satchi.notification:notification-source ntf)))
            (w (satchi.notification:source-name source)
               2 (incf y))
            (w (satchi.notification:source-url source)
               2 (incf y)))
          (incf y))))))
