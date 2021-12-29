(defpackage :satchi.filter
  (:use :cl)
  (:export :state
           :update-mentioned
           :update-keyword
           :toggle-mentioned
           :change-keyword))
(in-package :satchi.filter)

(defclass state () ())
(defgeneric update-mentioned (state fn))
(defgeneric update-keyword (state str))

(defun toggle-mentioned (&key state presenter)
  (update-mentioned state #'not)
  (satchi.presenter:update presenter))

(defun change-keyword (keyword &key state presenter)
  (update-keyword state keyword)
  (satchi.presenter:update presenter))
