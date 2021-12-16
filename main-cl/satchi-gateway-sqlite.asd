(asdf:defsystem :satchi-gateway-sqlite
  :serial t
  :pathname "src"
  :components
  ((:file "gateway/sqlite"))
  :depends-on (:satchi
               :sqlite))
