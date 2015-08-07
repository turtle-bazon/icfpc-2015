
(in-package :hextris)

(defstruct cell
  (row 0 :type fixnum)
  (col 0 :type fixnum))

(defclass hextris-map ()
  ((width :initarg :width :reader width)
   (height :initarg :height :reader height)
   (field :initarg :field :accessor field)))

(defmethod initialize-instance :after ((obj hextris-map) &key width height &allow-other-keys)
  (setf (field obj) (make-array (* width height) :element-type 'bit))
  nil)

(defmethod map-cell ((obj hextris-map) (c cell))
  (when (and (>= (cell-row c) 0) (< (cell-row c) (height obj)) (>= (cell-col c) 0) (< (cell-col c) (width obj)))
    (values c (not (zerop (elt (field obj) (+ (* (cell-row c) (height obj)) (cell-col c))))))))

(defmethod (setf map-cell) (value (obj hextris-map) (c cell))
  (setf (elt (field obj) (+ (* (cell-row c) (height obj)) (cell-col c)))
        (if value 1 0))
  (map-cell obj c))

(defparameter *moves* '(:nw :ne :e :se :sw :w))
(defparameter *rotations* '(:rcw :rcc))

(defmethod neighbour ((move (eql :nw)) (obj hextris-map) (c cell))
  (map-cell obj (make-cell :row (1- (cell-row c)) :col (if (evenp (cell-row c)) (1- (cell-col c)) (cell-col c)))))
(defmethod neighbour ((move (eql :ne)) (obj hextris-map) (c cell))
  (map-cell obj (make-cell :row (1- (cell-row c)) :col (if (evenp (cell-row c)) (cell-col c) (1+ (cell-col c))))))
(defmethod neighbour ((move (eql :e)) (obj hextris-map) (c cell))
  (map-cell obj (make-cell :row (cell-row c) :col (1+ (cell-col c)))))
(defmethod neighbour ((move (eql :se)) (obj hextris-map) (c cell))
  (map-cell obj (make-cell :row (1+ (cell-row c)) :col (if (evenp (cell-row c)) (cell-col c) (1+ (cell-col c))))))
(defmethod neighbour ((move (eql :sw)) (obj hextris-map) (c cell))
  (map-cell obj (make-cell :row (1+ (cell-row c)) :col (if (evenp (cell-row c)) (1- (cell-col c)) (cell-col c)))))
(defmethod neighbour ((move (eql :w)) (obj hextris-map) (c cell))
  (map-cell obj (make-cell :row (cell-row c) :col (1- (cell-col c)))))

(defmethod neighbours ((obj hextris-map) (c cell))
  (remove-if-not #'identity (mapcar (lambda (move) (neighbour move obj c)) *moves*)))


      
