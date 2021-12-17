(asdf:defsystem :satchi
  :serial t
  :pathname "src"
  :components
  ((:file "notification")
   (:file "time-machine")
   (:file "desktop-notification")
   (:file "notification-list")
   (:file "view-latest")
   (:file "filter")
   (:file "gateway")
   (:file "view")
   (:file "main"))
  :depends-on (:jsown
               :local-time))
