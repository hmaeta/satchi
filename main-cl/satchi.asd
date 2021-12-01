(asdf:defsystem :satchi
  :serial t
  :pathname "src"
  :components
  ((:file "notification")
   (:file "client/rss")
   (:file "notification-list")
   (:file "filter")
   (:file "main"))
  :depends-on (:jsown
               :dexador
               :quri
               :local-time
               :do-urlencode
               :rss))
