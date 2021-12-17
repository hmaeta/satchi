(defpackage :satchi.view.ncurses-paint
  (:use :cl)
  (:export :run))
(in-package :satchi.view.ncurses-paint)

(defgeneric run (view wnd))

(defmethod run ((v satchi.view.ncurses:loading) wnd)
  (multiple-value-bind (width height)
      (charms:window-dimensions wnd)
    (let* ((message "Loading...")
           (length/2 (floor (length message) 2)))
      (charms:write-string-at-point wnd
                                    message
                                    (- (floor width 2) length/2)
                                    (floor height 2)))))

(defmethod run ((v satchi.view.ncurses:viewing) wnd)
  (labels ((w (x y str)
             (charms:write-string-at-point wnd str x y)))
    (let ((ntfs (satchi.view.ncurses:viewing-ntfs v))
          (y -1))
      (dolist (ntf ntfs)
        (w 0 (incf y) (satchi.view.ncurses:ntf-title ntf))
        (w 2 (incf y) (satchi.view.ncurses:ntf-message ntf))
        (incf y)
        (w 2 (incf y) (satchi.view.ncurses:ntf-timestamp ntf))
        (w 2 (incf y) (satchi.view.ncurses:ntf-source-name ntf))
        (w 2 (incf y) (satchi.view.ncurses:ntf-source-url ntf))
        (incf y)))))
