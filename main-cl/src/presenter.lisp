(defpackage :satchi.presenter
  (:use :cl)
  (:export :presenter
           :update))
(in-package :satchi.presenter)

(defclass presenter ()
  ((update-fn
    :initarg :update-fn)))

(defun update (presenter)
  (funcall (slot-value presenter 'update-fn)))
