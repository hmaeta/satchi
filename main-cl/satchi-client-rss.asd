(asdf:defsystem :satchi-client-rss
  :serial t
  :pathname "src"
  :components
  ((:file "client/rss"))
  :depends-on (:rss
               :babel
               :drakma
               :satchi))
