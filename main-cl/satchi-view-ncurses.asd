(asdf:defsystem :satchi-view-ncurses
  :serial t
  :pathname "src"
  :components
  ((:file "view/ncurses")
   (:file "view/ncurses-ui"))
  :depends-on (:cl-charms
               :cl-setlocale))
