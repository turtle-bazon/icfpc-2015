
(in-package :hextris)

(defparameter *a-star-moves* '(:w :sw :se :e :rotate-cw :rotate-ccw))

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
        (make-unit-on-map :unit (make-instance 'unit
                                               :pivot (pivot (unit-on-map-unit obj))
                                               :members (members (unit-on-map-unit obj)))
                          :coord (unit-on-map-coord obj))))

(defmethod run-a-star ((field hextris-map) (start-pos unit-on-map) (end-pos unit-on-map))
  (let ((queue (make-instance 'basic-queue))
        (visited (make-instance 'binary-search-tree :sorter #'positions< :test #'positions=)))
    (enqueue queue (list start-pos '()))
    (insert-item visited start-pos)
    (iter (until (empty-p queue))
          (for (current-pos commands) = (dequeue queue))
          (when (positions= current-pos end-pos)
            (return-from run-a-star (values t (nreverse commands))))
          (for transitions = (remove-if-not #'identity (mapcar (lambda (move) (move-unit move current-pos field)) *a-star-moves*)))
          (iter (for (move . next-pos) in transitions)
                (unless (find-item visited next-pos)
                  (enqueue queue (list next-pos (cons move commands)))
                  (insert-item visited next-pos))))))
          
          
