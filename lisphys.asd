;;;; lisphys.asd

(asdf:defsystem #:lisphys
  :serial t
  :description "Describe lisphys here"
  :author "Guillaume <guillaume.saupin@gmail.com>"
  :license "Specify license here"
  :depends-on (#:clunit
	       #:alexandria
	       #:cl-opengl
	       #:cl-glu
	       #:lispbuilder-sdl)
  :components ((:file "infpre")
	       (:file "package")
	       (:file "memoization")
               (:file "autodiff")
               (:file "autodiff-test")
               (:file "math")
               (:file "vector3")
               (:file "vector3-test")
               (:file "vector3ad")
               (:file "vector3ad-test")
               (:file "quaternion")
               (:file "quaternionad")
               (:file "quaternionad-test")
               (:file "twist")
               (:file "displacement")
	       (:file "matrix")
	       (:file "matrix-test")
	       (:file "viewer")
               (:file "lisphys")))

