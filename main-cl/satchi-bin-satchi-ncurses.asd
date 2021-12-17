(asdf:defsystem :satchi-bin-satchi-ncurses
  :serial t
  :pathname "src"
  :components
  ((:file "bin/satchi-ncurses"))
  :depends-on (:satchi
               :satchi-view-ncurses
               :satchi-gateway-memory
               :satchi-client-rss
               :cl-setlocale))
