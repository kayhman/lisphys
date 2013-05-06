;;;; lisphys.asd

(asdf:defsystem #:lisphys
  :serial t
  :description "Describe lisphys here"
  :author "Guillaume <guillaume.saupin@gmail.com>"
  :license "Specify license here"
  :components ((:file "package")
               (:file "autodiff")
               (:file "math")
               (:file "vector3")
               (:file "vector3ad")
               (:file "quaternion")
               (:file "quaternionad")
               (:file "lisphys")))

