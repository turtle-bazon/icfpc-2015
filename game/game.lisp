
(in-package :hextris)

(defclass game ()
  ((map :initarg :map :accessor game-map)
   (units :initarg :units :reader units)
   (seeds :initarg :seeds :reader seeds)))

