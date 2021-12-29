(asdf:defsystem :satchi
  :serial t
  :pathname "src"
  :components
  ((:file "notification")
   (:file "client")
   (:file "presenter")
   (:file "notification-holder")
   (:file "time-machine")
   (:file "desktop-notification")
   (:file "notification-list")
   (:file "filter")
   (:file "gateway")
   (:file "view-latest")
   (:file "main"))
  :depends-on (:local-time))
