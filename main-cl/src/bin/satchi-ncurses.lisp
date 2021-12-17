(defpackage :satchi.bin.satchi-ncurses
  (:use :cl)
  (:export :main))
(in-package :satchi.bin.satchi-ncurses)

(defvar *gws*
  (list (satchi.gateway:make-gateway
         :id "rss"
         :client
         (satchi.client.rss:make-client
          :url "https://news.yahoo.co.jp/rss/topics/top-picks.xml"))))

(defvar *send-view-fn* nil)

;;  ros run -s satchi-bin-satchi-ncurses -e '(satchi.bin.satchi-ncurses:main)' -q
(defun main ()
  (let ((view-needs-update nil)
        (lock (bt:make-lock "view-needs-update")))
    (setq *send-view-fn*
          (lambda (s)
            (declare (ignore s))
            (bt:with-lock-held (lock)
              (setf view-needs-update t))))
    (let* ((service (satchi:make-service
                     :gateways *gws*
                     :send-view-fn *send-view-fn*))
           (thread (bt:make-thread
                    (lambda ()
                      (sleep 3)
                      (satchi:view-latest service)))))
      (unwind-protect
           (charms:with-curses ()
             (charms:disable-echoing)
             (charms:enable-raw-input :interpret-control-characters t)
             (charms:enable-non-blocking-mode charms:*standard-window*)
             (let ((wnd charms:*standard-window*))
               (loop named loop
                     for c = (charms:get-char wnd :ignore-error t)
                     do (progn
                          (bt:with-lock-held (lock)
                            (when view-needs-update
                              (charms:clear-window wnd)
                              (satchi.view.ncurses:paint
                               (satchi:service-state service) wnd)
                              (charms:refresh-window wnd)
                              (setf view-needs-update nil)))
                          (case c
                            ((nil) nil)
                            ((#\q #\Q) (return-from loop)))))))
        (ignore-errors
         (bt:destroy-thread thread))))))
