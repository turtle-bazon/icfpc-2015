
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

(defun cell-move* (cell direction &optional (step 1))
  "Move cell destructively in direction (:e, :w, :se, :sw, :ne, :nw) by step cells"
  (when (member direction '(:w :se :sw))
    (setf step (- step)))
  (ecase direction
    ;; east/west(+/-N): (x, y, z) -> (x + N, y - N, z)
    ((:e :w)
     (incf (cell-cube-x cell) step)
     (decf (cell-cube-y cell) step))

    ;; north-west/south-east(+/-N): (x, y, z) -> (x, y + N, z - N)
    ((:nw :se)
     (incf (cell-cube-y cell) step)
     (decf (cell-cube-z cell) step))

    ;; north-east/south-west(+/-N): (x, y, z) -> (x + N, y, z - N)
    ((:ne :sw)
     (incf (cell-cube-x cell) step)
     (decf (cell-cube-z cell) step)))
  cell)

(defun cell-move (cell direction &optional (step 1))
  "Non-destructive version of cell-move*"
  (cell-move* (copy-cell cell) direction step))

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

(defmethod clone-map ((original-map hextris-map))
  (make-instance 'hextris-map :width (width original-map) :height (height original-map)
                 :field (copy-seq (field original-map))))

(defmethod map-burn-lines ((original-map hextris-map))
  (let ((width (width original-map))
        (height (height original-map))
        (field (field original-map)))
    (let* ((new-field (make-array (* width height) :element-type 'bit))
           (rows-deleted (iter (with row-cor = 0)
                               (for row from (1- height) downto 0)
                               (when (< (- row row-cor) 0)
                                 (terminate))
                               (iter (for fullness = (iter (for col from 0 below width)
                                                           (sum (elt field (+ (* (- row row-cor)
                                                                                 height) col)))))
                                     (while (= fullness width))
                                     (incf row-cor))
                               (iter (for col from 0 below width)
                                     (setf (elt new-field (+ (* row height) col))
                                           (elt field (+ (* (- row row-cor) height) col))))
                               (finally (return row-cor)))))
      (values (make-instance 'hextris-map :width width :height height :field new-field)
              rows-deleted))))


(defmethod count-free-cells (row (field hextris-map))
  (iter (for col from 0 below (width field))
        (for cell = (make-cell-row-col row col))
        (counting (multiple-value-bind (cell filled-p) (map-cell field cell)
                    (and cell (not filled-p))))))

(defmethod map-burn-lines-v2 ((field hextris-map))
  (let ((current-map (clone-map field)))
    (iter (for lowest-row from (1- (height current-map)) downto 1)
          (when (zerop (count-free-cells lowest-row current-map))
            (iter (for row from lowest-row downto 1)
                  (iter (for col from 0 below (width current-map))
                        (setf (map-cell current-map (make-cell-row-col row col))
                              (map-cell-free-p current-map (make-cell-row-col (1- row) col)))))
            (iter (for col from 0 below (width current-map))
                  (setf (map-cell current-map (make-cell-row-col 0 col)) nil))
            (incf lowest-row)))
    current-map))

(defmethod map-cell-free-p ((obj hextris-map) (c cell))
  (multiple-value-bind (row col) (cell-row-col c)
    (not (zerop (elt (field obj) (+ (* row (width obj)) col))))))

(defmethod map-cell-free-p* ((obj hextris-map) row col)
  (zerop (elt (field obj) (+ (* row (width obj)) col))))

(defmethod map-cell-free-p** ((obj hextris-map) (c cell))
  (multiple-value-bind (row col) (cell-row-col c)
    (map-cell-free-p* obj row col)))


(defmethod map-cell ((obj hextris-map) (c cell))
  (multiple-value-bind (row col) (cell-row-col c)
    (when (and (>= row 0) (< row (height obj)) (>= col 0) (< col (width obj)))
      (values c (map-cell-free-p obj c)))))

(defmethod (setf map-cell) (value (obj hextris-map) (c cell))
  (multiple-value-bind (row col) (cell-row-col c)
    (setf (elt (field obj) (+ (* row (width obj)) col))
          (if value 1 0))
    (map-cell obj c)))

(defmethod map-burn-lines-v3 ((original-map hextris-map))
  (bind ((width (width original-map))
         (height (height original-map))
         (field (field original-map))
         (new-field (make-array (* width height) :element-type 'bit))
         (rows-deleted 0))
    (iter (for row from (1- height) downto 0)
          (for burn-p = (iter (for col from 0 below width)
                              (never (map-cell-free-p* original-map col row))))

          (if burn-p
              (incf rows-deleted)
              (iter (for col from 0 below width)
                    (setf (elt new-field (+ (* row width) col))
                          (elt field (+ (* (- row rows-deleted) height) col))))))

    (values (make-instance 'hextris-map :width width :height height :field new-field)
            rows-deleted)))

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


      
