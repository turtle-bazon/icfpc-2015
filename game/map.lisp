
(in-package :hextris)

(defclass hextris-map ()
  ((width :initarg :width :reader width)
   (height :initarg :height :reader height)
   (field :initarg :field :accessor field)))

(defmethod initialize-instance :after ((obj hextris-map) &key width height &allow-other-keys)
  (setf (field obj) (make-array (* width height) :element-type 'bit))
  nil)

(defmethod cell ((obj hextris-map) row col)
  (when (and (>= row 0) (< row (height obj)) (>= col 0) (< col (width obj)))
    (list row col (not (zerop (elt (field obj) (+ (* row (height obj)) col)))))))

(defmethod (setf cell) (value (obj hextris-map) row col)
  (setf (elt (field obj) (+ (* row (height obj)) col))
        (if value 1 0))
  (cell obj row col))

(defparameter *moves* '(:nw :ne :e :se :sw :w))

(defmethod neighbour ((move (eql :nw)) (obj hextris-map) row col)
  (cell obj (1- row) (if (evenp row) (1- col) col)))
(defmethod neighbour ((move (eql :ne)) (obj hextris-map) row col)
  (cell obj (1- row) (if (evenp row) col (1+ col))))
(defmethod neighbour ((move (eql :e)) (obj hextris-map) row col)
  (cell obj row (1+ col)))
(defmethod neighbour ((move (eql :se)) (obj hextris-map) row col)
  (cell obj (1+ row) (if (evenp row) col (1+ col))))
(defmethod neighbour ((move (eql :sw)) (obj hextris-map) row col)
  (cell obj (1+ row) (if (evenp row) (1- col) col)))
(defmethod neighbour ((move (eql :w)) (obj hextris-map) row col)
  (cell obj row (1- col)))

(defmethod neighbours ((obj hextris-map) row col)
  (remove-if-not #'identity (mapcar (lambda (move) (neighbour move obj row col)) *moves*)))
      
