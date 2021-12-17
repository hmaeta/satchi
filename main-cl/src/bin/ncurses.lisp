(defpackage :satchi.bin.ncurses
  (:use :cl)
  (:export :main))
(in-package :satchi.bin.ncurses)

(defvar *threads* nil)

(defmethod satchi.view.ncurses.ui:view-latest ((s satchi:service))
  (push (bt:make-thread (lambda ()
                          (satchi:view-latest s)))
        *threads*))

(defun ui-main ()
  (satchi.view.ncurses.ui:main
   (lambda (update-view-fn)
     (satchi:make-service
      :gateways
      (list (satchi.gateway:make-gateway
             :id "rss"
             :client
             (satchi.client.rss:make-client
              :url "https://news.yahoo.co.jp/rss/topics/top-picks.xml")))
      :send-view-fn
      (lambda (service)
        (let ((state (satchi:service-state service)))
          (let ((view (satchi.view.ncurses:make state)))
            (funcall update-view-fn view))))))))

;;  ros run -s satchi-bin-ncurses -e '(satchi.bin.ncurses:main)' -q
(defun main ()
  (unwind-protect (ui-main)
    (mapc (lambda (th)
            (ignore-errors (bt:destroy-thread th)))
          *threads*)))
