(defpackage :satchi.filter
  (:use :cl)
  (:export :gui-update
           :state
           :update-mentioned
           :update-keyword
           :toggle-mentioned
           :change-keyword))
(in-package :satchi.filter)

(defgeneric gui-update (renderer))

(defclass state () ())
(defgeneric update-mentioned (state fn))
(defgeneric update-keyword (state str))

(defun toggle-mentioned (&key state renderer)
  (update-mentioned state #'not)
  (gui-update renderer))

(defun change-keyword (keyword &key state renderer)
  (update-keyword state keyword)
  (gui-update renderer))
