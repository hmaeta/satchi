(defpackage :satchi.view.ncurses.ui
  (:use :cl)
  (:export :view-incoming-notifications
           :view-latest
           :main))
(in-package :satchi.view.ncurses.ui)

(defgeneric view-latest (service))
(defgeneric view-incoming-notifications (service))

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
      (multiple-value-bind (width height)
          (charms:window-dimensions wnd)
        (declare (ignore width))
        (loop for ntf in ntfs while (< (+ y 6) height)
              do (progn
                   (w 0 (incf y) (satchi.view.ncurses:ntf-title ntf))
                   (w 2 (incf y) (satchi.view.ncurses:ntf-message ntf))
                   (incf y)
                   (w 2 (incf y) (satchi.view.ncurses:ntf-timestamp ntf))
                   (w 2 (incf y) (satchi.view.ncurses:ntf-source-name ntf))
                   (w 2 (incf y) (satchi.view.ncurses:ntf-source-url ntf))
                   (incf y)))))))

(defun main (make-service)
  ;; Japanese chacacters are written properly
  (cl-setlocale:set-all-to-native)

  (unwind-protect
       (charms:with-curses ()
         (charms:disable-echoing)
         (charms:enable-raw-input :interpret-control-characters t)
         (charms:enable-non-blocking-mode charms:*standard-window*)
         (let ((wnd charms:*standard-window*)
               (view-list nil)
               (view-list-lock (bt:make-lock "view-list")))
           (labels ((update-view (view)
                      (bt:with-lock-held (view-list-lock)
                        (setf view-list (append view-list (list view))))))
             (let ((service (funcall make-service #'update-view)))
               (when service
                 (view-latest service)
                 (loop named loop
                       for c = (charms:get-char wnd :ignore-error t)
                       do (progn
                            (bt:with-lock-held (view-list-lock)
                              (when view-list
                                (let ((view (pop view-list)))
                                  (charms:clear-window wnd)
                                  (paint view wnd)
                                  (charms:refresh-window wnd))))
                            (case c
                              ((#\r #\R)
                               (view-incoming-notifications service))
                              ((#\q #\Q)
                               (return-from loop))))))))))))
