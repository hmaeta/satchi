(asdf:defsystem :satchi-bin-server
  :serial t
  :pathname "src"
  :components
  ((:file "bin/server"))
  :depends-on (:clack
               :websocket-driver
               :do-urlencode
               :quri
               :bordeaux-threads
               :satchi))
