(asdf:defsystem :satchi-bin-ncurses
  :serial t
  :pathname "src"
  :components
  ((:file "view/ncurses-from-state")
   (:file "bin/ncurses"))
  :depends-on (:satchi
               :satchi-view-ncurses
               :satchi-gateway-memory
               :satchi-client-rss))
