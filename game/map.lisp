
(in-package :hextris)

(defstruct cell
  (cube-x 0 :type fixnum)
  (cube-y 0 :type fixnum)
  (cube-z 0 :type fixnum))

(defun make-cell-row-col (row col)
  (let* ((cube-x (- col (truncate (- row (mod row 2)) 2)))
         (cube-z row)
         (cube-y (- 0 cube-x cube-z)))
    (make-cell :cube-x cube-x :cube-y cube-y :cube-z cube-z)))

(defun cell-row-col (cell)
  (values (cell-cube-z cell)
          (+ (cell-cube-x cell)
             (truncate (- (cell-cube-z cell) (mod (cell-cube-z cell) 2)) 2))))


(deftestsuite cell-tests () ())

(defrandom-instance a-coord nil (- (random 200) 100))

(addtest (cell-tests)
  cell-cube-identity

  (ensure-random-cases 100 ((row a-coord) (col a-coord))
    (bind ((cell (make-cell-row-col row col))
           ((:values row* col*) (cell-row-col cell)))
      (ensure-same row row*)
      (ensure-same col col*)))

  ;; TODO: Decide does we need this part at all?

  ;; (ensure-random-cases 100 ((x a-coord) (y a-coord) (z a-coord))
  ;;   (bind ((cell (make-cell :cube-x x :cube-y y :cube-z z))
  ;;          (x* (cell-cube-x cell))
  ;;          (y* (cell-cube-y cell))
  ;;          (z* (cell-cube-z cell)))

  ;;     (cell-update-offset cell)
  ;;     (cell-update-cube cell)
  ;;     (ensure-same x* (cell-cube-x cell))
  ;;     ;; This is not required to be same (as i understood)
  ;;     ;; (ensure-same y* (cell-cube-y cell))
  ;;     (ensure-same z* (cell-cube-z cell))))
  )

(defun cell< (cell-a cell-b)
  (or (< (cell-cube-x cell-a) (cell-cube-x cell-b))
      (and (= (cell-cube-x cell-a) (cell-cube-x cell-b))
           (or (< (cell-cube-y cell-a) (cell-cube-y cell-b))
               (and (= (cell-cube-y cell-a) (cell-cube-y cell-b))
                    (< (cell-cube-z cell-a) (cell-cube-z cell-b)))))))
                    
(defun cell= (cell-a cell-b)
  (and (= (cell-cube-x cell-a) (cell-cube-x cell-b))
       (= (cell-cube-y cell-a) (cell-cube-y cell-b))
       (= (cell-cube-z cell-a) (cell-cube-z cell-b))))

(defclass hextris-map ()
  ((width :initarg :width :reader width)
   (height :initarg :height :reader height)
   (field :initarg :field :accessor field)))

(defmethod initialize-instance :after ((obj hextris-map) &key width height &allow-other-keys)
  (when (not (slot-boundp obj 'field))
    (setf (field obj) (make-array (* width height) :element-type 'bit)))
  nil)

(defmethod clone-map-with ((original-map hextris-map) field-modify-fn)
  (make-instance 'hextris-map :width (width original-map) :height (height original-map)
                 :field (funcall field-modify-fn
                                 (field original-map)
                                 (width original-map)
                                 (height original-map))))

(defmethod clone-map ((original-map hextris-map))
  (clone-map-with original-map (lambda (field width height)
                                 (declare (ignore width height))
                                 (copy-seq field))))

(defun field-copy ()
  (lambda (field width height)
    (declare (ignore width height))
    (copy-seq field)))

(defun field-burn-line (fields)
  (lambda (field width height)
    (let ((new-field (make-array (* width height) :element-type 'bit)))
      (iter (for col from 0 below width)
            (iter (for row from 0 below (- height fields))
                  (setf (elt new-field (+ (* (+ fields row) height) col))
                        (elt field (+ (* row height) col)))))
      new-field)))

(defmethod map-cell-free-p ((obj hextris-map) (c cell))
  (multiple-value-bind (row col) (cell-row-col c)
    (not (zerop (elt (field obj) (+ (* row (height obj)) col))))))

(defmethod map-cell ((obj hextris-map) (c cell))
  (multiple-value-bind (row col) (cell-row-col c)
    (when (and (>= row 0) (< row (height obj)) (>= col 0) (< col (width obj)))
      (values c (map-cell-free-p obj c)))))

(defmethod (setf map-cell) (value (obj hextris-map) (c cell))
  (multiple-value-bind (row col) (cell-row-col c)
    (setf (elt (field obj) (+ (* row (height obj)) col))
          (if value 1 0))
    (map-cell obj c)))

(defparameter *moves* '(:nw :ne :e :se :sw :w))
(defparameter *rotations* '(:rcw :rcc))

;;; TODO: to be fixed

;; (defmethod neighbour ((move (eql :nw)) (obj hextris-map) (c cell))
;;   (map-cell obj (make-cell :row (1- (cell-row c)) :col (if (evenp (cell-row c)) (1- (cell-col c)) (cell-col c)))))
;; (defmethod neighbour ((move (eql :ne)) (obj hextris-map) (c cell))
;;   (map-cell obj (make-cell :row (1- (cell-row c)) :col (if (evenp (cell-row c)) (cell-col c) (1+ (cell-col c))))))
;; (defmethod neighbour ((move (eql :e)) (obj hextris-map) (c cell))
;;   (map-cell obj (make-cell :row (cell-row c) :col (1+ (cell-col c)))))
;; (defmethod neighbour ((move (eql :se)) (obj hextris-map) (c cell))
;;   (map-cell obj (make-cell :row (1+ (cell-row c)) :col (if (evenp (cell-row c)) (cell-col c) (1+ (cell-col c))))))
;; (defmethod neighbour ((move (eql :sw)) (obj hextris-map) (c cell))
;;   (map-cell obj (make-cell :row (1+ (cell-row c)) :col (if (evenp (cell-row c)) (1- (cell-col c)) (cell-col c)))))
;; (defmethod neighbour ((move (eql :w)) (obj hextris-map) (c cell))
;;   (map-cell obj (make-cell :row (cell-row c) :col (1- (cell-col c)))))

;; (defmethod neighbours ((obj hextris-map) (c cell))
;;   (remove-if-not #'identity (mapcar (lambda (move) (neighbour move obj c)) *moves*)))


      
