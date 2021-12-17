(asdf:defsystem :satchi-view-ncurses
  :serial t
  :pathname "src"
  :components
  ((:file "view/ncurses")
   (:file "view/ncurses-paint"))
  :depends-on (:cl-charms
               :satchi))
