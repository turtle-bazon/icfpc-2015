
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

;;; TODO: find out optimization (without sort)
(defun positions< (pos-a pos-b)
  (or (cell< (unit-on-map-coord pos-a) (unit-on-map-coord pos-b))
      (and (cell= (unit-on-map-coord pos-a) (unit-on-map-coord pos-b))
           (iter (for cell-a in (position->vec pos-a))
                 (for cell-b in (position->vec pos-b))
                 (cond ((cell< cell-a cell-b) (return-from positions< t))
                       ((cell= cell-a cell-b) (next-iteration))
                       (t (return-from positions< nil)))))))

;;; TODO: implement actual movement
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
  
(defmethod run-a-star ((field hextris-map) (start-pos unit-on-map) (end-pos unit-on-map))
  (declare (optimize (debug 3)))
  (let ((queue (make-instance 'basic-queue))
        (visited (make-hash-table :test 'equal)))
    (enqueue queue (list start-pos '()))
    (setf (gethash (position->points-list start-pos) visited) t)
    (iter (until (empty-p queue))
          (for (current-pos commands) = (dequeue queue))
          (when (positions= current-pos end-pos)
            (return-from run-a-star (values t (reverse commands))))
          (for transitions = (remove-if-not #'identity (mapcar (lambda (move) (move-unit move current-pos field)) *a-star-moves*)))
          (iter (for (move . next-pos) in (sort transitions (position-better-p end-pos) :key #'cdr))
                (for hash-key = (position->points-list next-pos))
                (unless (gethash hash-key visited)
                  (when (place-on-map (unit-on-map-unit next-pos) (unit-on-map-coord next-pos) field)
                    (enqueue queue (list next-pos (cons move (copy-seq commands))))
                    (setf (gethash hash-key visited) t)))))))
          
          
