(asdf:defsystem :satchi-view-electron
  :serial t
  :pathname "src"
  :components
  ((:file "view/electron")
   (:file "view/electron-json"))
  :depends-on (:satchi
               :jsown))
