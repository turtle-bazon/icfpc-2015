
(in-package :hextris)

(defclass unit ()
  ((members :initarg :members :accessor members)
   (pivot :initarg :pivot :type cell :accessor pivot)))

