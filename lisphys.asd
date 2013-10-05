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
  :components ((:file "src/lib/infpre")
	       (:file "package")
	       (:file "src/lib/memoization")
               (:file "src/lib/autodiff")
               (:file "src/tests/autodiff-test")
               (:file "src/lib/math")
               (:file "src/lib/vector3")
               (:file "src/tests/vector3-test")
               (:file "src/lib/vector3ad")
               (:file "src/tests/vector3ad-test")
               (:file "src/lib/quaternion")
               (:file "src/lib/quaternionad")
               (:file "src/tests/quaternionad-test")
               (:file "src/lib/displacement")
               (:file "src/lib/twist")
               (:file "src/tests/twist-test")
	       (:file "src/lib/matrix")
	       (:file "src/lib/matrixad")
	       (:file "src/lib/inertia-matrix")
	       (:file "src/tests/matrix-test")
	       (:file "src/lib/jacobian")
	       (:file "src/lib/viewer")
	       (:file "src/lib/rigid-body-api")
	       (:file "src/tests/rigid-body-api-test")
	       (:file "src/examples/elbow-manipulator")
	       (:file "src/examples/three-links-manipulator-rb")
	       (:file "src/lib/lisphys")))

