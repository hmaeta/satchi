(defpackage :satchi.view.ncurses.ui
  (:use :cl)
  (:export :view-latest
           :main))
(in-package :satchi.view.ncurses.ui)

(defgeneric view-latest (service))

(defgeneric paint (v wnd))

(defmethod paint ((v satchi.view.ncurses:loading) wnd)
  (multiple-value-bind (width height)
      (charms:window-dimensions wnd)
    (let* ((message "Loading...")
           (length/2 (floor (length message) 2)))
      (charms:write-string-at-point wnd
                                    message
                                    (- (floor width 2) length/2)
                                    (floor height 2)))))

(defmethod paint ((v satchi.view.ncurses:viewing) wnd)
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

(defun main (make-service)
  ;; Japanese chacacters are written properly
  (cl-setlocale:set-all-to-native)

  (unwind-protect
       (charms:with-curses ()
         (charms:disable-echoing)
         (charms:enable-raw-input :interpret-control-characters t)
         (charms:enable-non-blocking-mode charms:*standard-window*)
         (let ((wnd charms:*standard-window*))
           (labels ((update-view (view)
                      (charms:clear-window wnd)
                      (paint view wnd)
                      (charms:refresh-window wnd)))
             (let ((service (funcall make-service #'update-view)))
               (view-latest service)
               (loop named loop
                     for c = (charms:get-char wnd :ignore-error t)
                     do (progn
                          (case c
                            ((#\r #\R)
                             (view-latest service))
                            ((#\q #\Q)
                             (return-from loop)))))))))))
