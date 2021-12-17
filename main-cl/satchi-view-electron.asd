(asdf:defsystem :satchi-view-electron
  :serial t
  :pathname "src"
  :components
  ((:file "view/electron"))
  :depends-on (:satchi
               :jsown))
