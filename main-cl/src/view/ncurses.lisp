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
  (multiple-value-bind (width height)
      (charms:window-dimensions wnd)
    (let* ((message "Viewing...")
           (length/2 (floor (length message) 2)))
      (charms:write-string-at-point wnd
                                    message
                                    (- (floor width 2) length/2)
                                    (floor height 2)))))
