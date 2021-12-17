(defpackage :satchi.view.ncurses
  (:use :cl)
  (:export :loading
           :viewing
           :viewing-ntfs
           :ntf-title
           :ntf-message
           :ntf-timestamp
           :ntf-source-name
           :ntf-source-url
           :make))
(in-package :satchi.view.ncurses)

(defstruct loading)

(defstruct ntf title message timestamp source-name source-url)
(defstruct viewing ntfs)
