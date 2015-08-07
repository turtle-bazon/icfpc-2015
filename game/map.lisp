
(in-package :hextris)

(defstruct cell
  (row 0 :type fixnum)
  (col 0 :type fixnum)
  (cube-x 0 :type fixnum)
  (cube-y 0 :type fixnum)
  (cube-z 0 :type fixnum))

(defun cell-update-cube (cell)
  "Update cube coordinates based on row/col"
  ;; convert odd-r offset to cube
  ;;   x = col - (row - (row&1)) / 2
  ;;   z = row
  ;;   y = -x-z
  (bind ((col (cell-col cell))
         (row (cell-row cell))
         (cube-x (- col
                    (/ (- row (mod row 2)) 2)))
         (cube-z row)
         (cube-y (- 0 cube-x cube-z)))

    (setf (cell-cube-x cell) cube-x
          (cell-cube-y cell) cube-y
          (cell-cube-z cell) cube-z)
    cell))

(defun cell-update-offset (cell)
  "Update row/col coordinates based on cube"
  ;; convert cube to odd-r offset
  ;;   col = x + (z - (z&1)) / 2
  ;;   row = z
  (bind ((cube-x (cell-cube-x cell))
         ;; (cube-y (cell-cube-y cell))
         (cube-z (cell-cube-z cell))
         (col (+ cube-x (/ (- cube-z (mod cube-z 2)) 2)))
         (row cube-z))
    (setf (cell-row cell) row
          (cell-col cell) col)
    cell))

(deftestsuite cell-tests () ())

(defrandom-instance a-coord nil (- (random 200) 100))

(addtest (cell-tests)
  cell-cube-identity

  (ensure-random-cases 100 ((row a-coord) (col a-coord))
    (bind ((cell (make-cell :row row :col col))
           (row* (cell-row cell))
           (col* (cell-col cell)))
      (cell-update-cube cell)
      (cell-update-offset cell)
      (ensure-same row (cell-row cell))
      (ensure-same col (cell-col cell))))

  (ensure-random-cases 100 ((x a-coord) (y a-coord) (z a-coord))
    (bind ((cell (make-cell :cube-x x :cube-y y :cube-z z))
           (x* (cell-cube-x cell))
           (y* (cell-cube-y cell))
           (z* (cell-cube-z cell)))

      (cell-update-offset cell)
      (cell-update-cube cell)
      (ensure-same x* (cell-cube-x cell))
      ;; This is not required to be same (as i understood)
      ;; (ensure-same y* (cell-cube-y cell))
      (ensure-same z* (cell-cube-z cell)))))

(defun cell< (cell-a cell-b)
  (or (< (cell-row cell-a) (cell-row cell-b))
      (and (= (cell-row cell-a) (cell-row cell-b))
           (< (cell-col cell-a) (cell-col cell-b)))))

(defun cell= (cell-a cell-b)
  (and (= (cell-row cell-a) (cell-row cell-b))
       (= (cell-col cell-a) (cell-col cell-b))))

(defclass hextris-map ()
  ((width :initarg :width :reader width)
   (height :initarg :height :reader height)
   (field :initarg :field :accessor field)))

(defmethod initialize-instance :after ((obj hextris-map) &key width height &allow-other-keys)
  (setf (field obj) (make-array (* width height) :element-type 'bit))
  nil)

(defmethod clone-map-with ((original-map hextris-map) field-modify-fn)
  (make-instance 'hextris-map :width (width original-map) :height (height original-map)
                 :field (funcall field-modify-fn (field original-map)
                                 (width original-map)
                                 (height original-map))))

(defmethod clone-map ((original-map hextris-map))
  (clone-map-with original-map (lambda (field width height) (copy-seq field))))

(defun field-burn-line (field widht height)
  (let ((new-field (copy-seq field)))
    (iter (for x from 0 below width)
          (iter (for y from 1 below height)
                nil))
    new-field))

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


      
