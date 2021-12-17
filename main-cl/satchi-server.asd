(asdf:defsystem :satchi-server
  :serial t
  :pathname "src"
  :components
  ((:file "server"))
  :depends-on (:jsown
               :satchi))
