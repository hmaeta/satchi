(defpackage :satchi.bin.ncurses-client
  (:use :cl)
  (:export :main))
(in-package :satchi.bin.ncurses-client)

(defstruct client ws)

(defmethod satchi.view.ncurses.ui:view-latest ((client client))
  (wsd:send (client-ws client)
            (jsown:to-json
             (jsown:new-js
               ("op" "Notifications")))))

(defun setup-ws (ws update-view-fn)
  (wsd:start-connection ws)
  (wsd:on :message ws
          (lambda (msg)
            (let* ((json (jsown:parse msg))
                   (type (jsown:val json "type")))
              (cond ((string= type "UpdateView")
                     (let ((view (satchi.view.ncurses:make
                                  (jsown:val json "value"))))
                       (funcall update-view-fn view))))))))

(defun main ()
  (let ((ws-list nil))
    (unwind-protect
         (satchi.view.ncurses.ui:main
          (lambda (update-view-fn)
            ;; electron server
            (let ((ws (wsd:make-client "ws://localhost:8037/connect")))
              (setup-ws ws update-view-fn)
              (push ws ws-list)
              (make-client :ws ws))))
      (dolist (ws ws-list)
        (ignore-errors (wsd:close-connection ws))))))
