(asdf:defsystem :satchi-bin-satchi
  :serial t
  :pathname "src"
  :components
  ((:file "bin/satchi"))
  :depends-on (:do-urlencode
               :satchi
               :satchi-server
               :satchi-gateway-memory
               :satchi-view-electron
               :satchi-client-rss))
