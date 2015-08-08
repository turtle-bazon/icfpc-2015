
(in-package :hextris)

(defparameter *a-star-moves* '(:w :sw :se :e :rcw :rcc))

(defstruct unit-on-map
  (unit nil :type (or null unit))
  (coord nil :type (or null cell)))

(defun cell->list (cell)
  (list (cell-cube-x cell) (cell-cube-y cell) (cell-cube-z cell)))

(defun position->points-list (pos)
  (cons (cell->list (unit-on-map-coord pos))
        (mapcar #'cell->list (position->vec pos))))

(defun position->vec (pos)
  (sort (copy-seq (members (unit-on-map-unit pos))) #'cell<))

;;; TODO: find out optimization (without sort)
(defun positions= (pos-a pos-b)
  (and (cell= (unit-on-map-coord pos-a) (unit-on-map-coord pos-b))
       (every #'cell= (position->vec pos-a) (position->vec pos-b))))

(defmethod move-unit (move (obj unit-on-map) (field hextris-map))
  (cons move
        (ecase move
          ((:w :sw :se :e)
           (make-unit-on-map :unit (unit-on-map-unit obj) :coord (cell-move (unit-on-map-coord obj) move)))
          ((:rcw :rcc)
           (make-unit-on-map :unit (unit-rotate (unit-on-map-unit obj) move) :coord (unit-on-map-coord obj))))))

(defun sq-dist (cell-a cell-b)
  (+ (* (- (cell-cube-x cell-b) (cell-cube-x cell-a))
        (- (cell-cube-x cell-b) (cell-cube-x cell-a)))
     (* (- (cell-cube-y cell-b) (cell-cube-y cell-a))
        (- (cell-cube-y cell-b) (cell-cube-y cell-a)))
     (* (- (cell-cube-z cell-b) (cell-cube-z cell-a))
        (- (cell-cube-z cell-b) (cell-cube-z cell-a)))))

(defmethod position-better-p (end-pos)
  (lambda (pos-a pos-b)
    (< (sq-dist (unit-on-map-coord pos-a) (unit-on-map-coord end-pos))
       (sq-dist (unit-on-map-coord pos-b) (unit-on-map-coord end-pos)))))

(defclass visited-cache ()
  ((level-1 :initform (make-hash-table :test 'eql) :reader level-1)))

(defun cell-sxhash (cell)
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type cell cell))
  (sxhash (list (cell-cube-x cell) (cell-cube-y cell) (cell-cube-z cell))))

(defun position-sxhash (pos)
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type unit-on-map pos))
  (let ((seed (cell-sxhash (unit-on-map-coord pos))))
    (declare (type (unsigned-byte 62) seed))
    (iter (declare (declare-variables))
          (for cell in (members (unit-on-map-unit pos)))
          (for (the (unsigned-byte 62) hash) = (cell-sxhash cell))
          (setf seed (logxor seed hash)))
    seed))

(defmethod mark-visited ((cache visited-cache) (pos unit-on-map) &key level-1-key)
  (let ((level-1-key (or level-1-key (position-sxhash pos))))
    (setf (gethash level-1-key (level-1 cache)) t)))

(defmethod fast-check ((cache visited-cache) (pos unit-on-map))
  (let ((level-1-key (position-sxhash pos)))
    (values (gethash level-1-key (level-1 cache)) level-1-key)))

(defmethod run-a-star ((field hextris-map) (start-pos unit-on-map) (end-pos unit-on-map))
  (declare (optimize (debug 3)))
  (let ((queue (make-instance 'basic-queue))
        (visited (make-instance 'visited-cache)))
    (enqueue queue (list start-pos '()))
    (mark-visited visited start-pos)
    (iter (until (empty-p queue))
          (for (current-pos commands) = (dequeue queue))
          (when (positions= current-pos end-pos)
            (return-from run-a-star (values t (reverse commands))))
          (for transitions = (remove-if-not #'identity (mapcar (lambda (move) (move-unit move current-pos field)) *a-star-moves*)))
          (iter (for (move . next-pos) in (sort transitions (position-better-p end-pos) :key #'cdr))
                (multiple-value-bind (visited-p level-1-key)
                    (fast-check visited next-pos)
                  (unless visited-p
                    (when (place-on-map (unit-on-map-unit next-pos) (unit-on-map-coord next-pos) field)
                      (enqueue queue (list next-pos (cons move (copy-seq commands))))
                      (mark-visited visited next-pos :level-1-key level-1-key))))))))
          
          
