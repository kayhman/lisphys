(defclass matrix (math)
  ((val :accessor matrix-val :initarg :val :initform (vector 1 2 3)))
  )
