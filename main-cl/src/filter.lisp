(defpackage :satchi.filter
  (:use :cl)
  (:export :make-state
           :service-update
           :toggle-mentioned))
(in-package :satchi.filter)

(defstruct state
  is-mention-only)

(defgeneric service-update (service fn))

(defun toggle-mentioned (service)
  (service-update service
   (lambda (s)
     (make-state
      :is-mention-only
      (not (state-is-mention-only s))))))
