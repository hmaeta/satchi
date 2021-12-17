(asdf:defsystem :satchi-view-ncurses
  :serial t
  :pathname "src"
  :components
  ((:file "view/ncurses"))
  :depends-on (:cl-charms
               :satchi))
