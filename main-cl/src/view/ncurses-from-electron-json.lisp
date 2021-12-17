(in-package :satchi.view.ncurses)

(defun make (json)
  (let ((class (jsown:val json "stateClass")))
    (cond ((string= class "LoadingState")
           (make-loading))
          ((string= class "ViewingState")
           (let ((data (jsown:val json "stateData")))
             (make-viewing
              :ntfs
              (loop for n in (jsown:val data "notifications")
                    for s = (jsown:val n "source")
                    collect (make-ntf
                             :title (jsown:val n "title")
                             :message (jsown:val n "message")
                             :timestamp (jsown:val n "timestamp")
                             :source-name (jsown:val s "name")
                             :source-url (jsown:val s "url"))))))
          (t (assert nil)))))
