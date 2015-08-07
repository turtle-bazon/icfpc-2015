
(in-package :hextris)

(defparameter *a-star-moves* '(:w :sw :se :e :rcw :rcc))

(defstruct unit-on-map
  (unit nil :type (or null unit))
  (coord nil :type (or null cell)))

(defun position->vec (pos)
  (sort (cons (unit-on-map-coord pos) (members (unit-on-map-unit pos))) #'cell<))

;;; TODO: find out optimization (without sort)
(defun positions= (pos-a pos-b) 
  (every #'cell= (position->vec pos-a) (position->vec pos-b)))

;;; TODO: find out optimization (without sort)
(defun positions< (pos-a pos-b)
  (iter (for cell-a in (position->vec pos-a))
        (for cell-b in (position->vec pos-b))
        (cond ((cell< cell-a cell-b) (return-from positions< t))
              ((cell= cell-a cell-b) (next-iteration))
              (t (return-from positions< nil)))))

;;; TODO: implement actual movement
(defmethod move-unit (move (obj unit-on-map) (field hextris-map))
  (cons move
        (ecase move
          ((:w :sw :se :e)
           (make-unit-on-map :unit (unit-on-map-unit obj) :coord (cell-move (unit-on-map-coord obj) move)))
          ((:rcw :rcc)
           (make-unit-on-map :unit (unit-rotate (unit-on-map-unit obj) move) :coord (unit-on-map-coord obj))))))

(defmethod position-better-p (end-pos)
  (lambda (pos-a pos-b)
    (flet ((sq-dist (cell-a cell-b)
             (+ (* (- (cell-cube-x cell-b) (cell-cube-x cell-a))
                   (- (cell-cube-x cell-b) (cell-cube-x cell-a)))
                (* (- (cell-cube-y cell-b) (cell-cube-y cell-a))
                   (- (cell-cube-y cell-b) (cell-cube-y cell-a)))
                (* (- (cell-cube-z cell-b) (cell-cube-z cell-a))
                   (- (cell-cube-z cell-b) (cell-cube-z cell-a))))))
      (< (sq-dist (unit-on-map-coord pos-a) (unit-on-map-coord end-pos))
         (sq-dist (unit-on-map-coord pos-b) (unit-on-map-coord end-pos))))))
  
(defmethod run-a-star ((field hextris-map) (start-pos unit-on-map) (end-pos unit-on-map))
  (declare (optimize (debug 3)))
  (let ((queue (make-instance 'basic-queue))
        (visited (make-instance 'binary-search-tree :sorter #'positions< :test #'positions=)))
    (enqueue queue (list start-pos '()))
    (insert-item visited start-pos)
    (iter (until (empty-p queue))
          (for (current-pos commands) = (dequeue queue))
          (when (positions= current-pos end-pos)
            (return-from run-a-star (values t (nreverse commands))))
          (for transitions = (remove-if-not #'identity (mapcar (lambda (move) (move-unit move current-pos field)) *a-star-moves*)))
          (iter (for (move . next-pos) in (sort transitions (position-better-p end-pos) :key #'cdr))
                (unless (find-item visited next-pos)
                  (enqueue queue (list next-pos (cons move commands)))
                  (insert-item visited next-pos))))))
          
          
