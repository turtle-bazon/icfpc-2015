
(in-package :hextris)

(defclass hextris-map ()
  ((width :initarg :width :reader width)
   (height :initarg :height :reader height)
   (field :initarg :field :accessor field)))

(defmethod initialize-instance :after ((obj hextris-map) &key width height &allow-other-keys)
  (setf (field obj) (make-array (* width height) :element-type 'bit))
  nil)

(defmethod cell ((obj hextris-map) row col)
  (not (zerop (elt (field obj) (+ (* row (height obj)) col)))))

(defmethod (setf cell) (value (obj hextris-map) row col)
  (setf (elt (field obj) (+ (* row (height obj)) col))
        (if value 1 0))
  obj)

;; (defmethod neighbours ((obj hextris-map) row col)
  
