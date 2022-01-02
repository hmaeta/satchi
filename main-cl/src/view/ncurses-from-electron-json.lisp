(in-package :satchi.view.ncurses)

(defun string-take (str len)
  (subseq str 0 (min (length str) len)))

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
                             :title
                             (string-take (jsown:val n "title") 160)
                             :message
                             (string-take (jsown:val n "message") 160)
                             :timestamp (jsown:val n "timestamp")
                             :source-name (jsown:val s "name")
                             :source-url (jsown:val s "url"))))))
          (t (assert nil)))))
