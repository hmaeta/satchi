(asdf:defsystem :satchi-gateway-memory
  :serial t
  :pathname "src"
  :components
  ((:file "gateway/memory"))
  :depends-on (:satchi))
