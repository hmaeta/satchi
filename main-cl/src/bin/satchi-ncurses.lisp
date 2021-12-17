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
  ;; Japanese chacacters are written properly
  (cl-setlocale:set-all-to-native)

  (unwind-protect
       (charms:with-curses ()
         (charms:disable-echoing)
         (charms:enable-raw-input :interpret-control-characters t)
         (charms:enable-non-blocking-mode charms:*standard-window*)
         (let ((wnd charms:*standard-window*))
           (labels ((update-view (service)
                      (charms:clear-window wnd)
                      (satchi.view.ncurses:paint
                       (satchi:service-state service)
                       wnd)
                      (charms:refresh-window wnd)))
             (let* ((service (satchi:make-service
                              :gateways *gws*
                              :send-view-fn #'update-view))
                    (threads (list (bt:make-thread
                                    (lambda ()
                                      (satchi:view-latest service))))))
               (loop named loop
                     for c = (charms:get-char wnd :ignore-error t)
                     do (progn
                          (case c
                            ((#\r #\R)
                             (push (bt:make-thread
                                    (lambda ()
                                      (satchi:view-latest service)))
                                   threads))
                            ((#\q #\Q)
                             (return-from loop)))))
               (mapc (lambda (th)
                       (ignore-errors (bt:destroy-thread th)))
                     threads)))))))
