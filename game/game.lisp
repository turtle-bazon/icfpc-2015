
(in-package :hextris)

(defclass game ()
  ((map :initarg :map :accessor game-map)
   (seeds :initarg :seeds :reader seeds)))

