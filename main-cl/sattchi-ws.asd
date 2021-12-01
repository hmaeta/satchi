(asdf:defsystem :satchi-ws
  :serial t
  :pathname "src"
  :components
  ((:file "ws"))
  :depends-on (:clack
               :websocket-driver
               :satchi))
