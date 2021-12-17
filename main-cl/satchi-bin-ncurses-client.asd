(asdf:defsystem :satchi-bin-ncurses-client
  :serial t
  :pathname "src"
  :components
  ((:file "view/ncurses-from-electron-json")
   (:file "bin/ncurses-client"))
  :depends-on (:satchi-view-ncurses
               :websocket-driver
               :jsown))
