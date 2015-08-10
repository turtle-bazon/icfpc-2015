
(in-package :hextris)

(defclass solver ()
  ())

(defgeneric estimate (solver field checking-pos &key translated-pos))

  
