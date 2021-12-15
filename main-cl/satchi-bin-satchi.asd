(asdf:defsystem :satchi-bin-satchi
  :serial t
  :pathname "src"
  :components
  ((:file "bin/satchi"))
  :depends-on (:do-urlencode
               :satchi
               :satchi-client-rss))
