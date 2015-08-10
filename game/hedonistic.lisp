
(in-package :hextris)

(defclass hedonistic-solver (solver)
  ((sum-of-heights-factor :initform 2.75 :reader sum-of-heights-factor)
   (row-burn-factor :initform 1.0 :reader row-burn-factor)
   (one-burnt-row-penalty :initform -5.0 :reader one-burnt-row-penalty)
   (blockade-factor :initform -7.5 :reader blockade-factor)
   (touching-something-factor :initform 3.0 :reader touching-something-factor)
   (touching-wall-factor :initform 3.5 :reader touching-wall-factor)
   (touching-floor-factor :initform 5.0 :reader touching-floor-factor)
   (row-fill-factor :initform 10 :reader row-fill-factor)
   (distict-power-words-count-factor :initform 0.2 :reader distict-power-words-count-factor)
   (total-power-words-count-factor :initform 0.1 :reader total-power-words-count-factor)))

(defun count-rows-fill (field)
  (iter (for row from 0 below (height field))
        (for (row-count row-sum) = (iter (with cont = 0)
                                         (with sum = 0)
                                         (with count = 0)
                                         (for col from 0 below (width field))
                                         (for cv = (not (map-cell-free-p* field row col)))
                                         (if cv
                                             (progn
                                               (incf count)
                                               (incf cont)
                                               (incf sum cont))
                                             (setf cont 0))
                                         (finally (return (list count sum)))))
        (for row-estimate = (/ (+ row-sum row)
                               (expt 2 (- (height field) row 1))))
        (sum row-estimate)))

(defun find-out-sum-of-heights (field)
  (iter (with heights = 0)
        (for col from 0 below (width field))
        (iter (for row from 0 below (height field))
              (unless (map-cell-free-p* field row col)
                (incf heights (- (height field) row))
                (terminate)))
        (finally (return heights))))

(defun find-out-burned-rows-count (field)
  (multiple-value-bind (map-with-burned-rows burned-rows-count)
      (map-burn-lines-v2 field)
    (declare (ignore map-with-burned-rows))
    (cond ((zerop burned-rows-count) 0.0)
          ((= burned-rows-count 1) *one-burnt-row-penalty*)
          (t (ash 2 burned-rows-count)))))

(defun find-out-blockades-count-col (field col)
  (iter (with blockades-count = 0)
        (with blocked-p = nil)
        (for row from 0 below (height field))
        (if (map-cell-free-p* field row col)
            (when blocked-p
              (incf blockades-count))
            (setf blocked-p t))
        (finally (return blockades-count))))

(defun find-out-blockades-count (field)
  (iter (for col from 0 below (width field))
        (summing (find-out-blockades-count-col field col))))

(defparameter *cell-neighbours* '(:w :sw :se :e :ne :nw))

(defun members-matches-counter (pos field predicate)
  (iter (with matches-count = 0)
        (for mem in (members pos))
        (iter (for direction in *cell-neighbours*)
              (for neighbour = (cell-move mem direction))
              (multiple-value-bind (cell filled-p)
                  (map-cell field neighbour)
                (when (funcall predicate neighbour cell filled-p)
                  (incf matches-count))))
        (finally (return matches-count))))

(defun members-touching-something-count (pos field)
  (members-matches-counter
   pos field
   (lambda (neighbour cell filled-p)
     (declare (ignore neighbour))
     (and cell filled-p))))

(defun members-touching-wall-count (pos field)
  (members-matches-counter
   pos field
   (lambda (neighbour cell filled-p)
     (declare (ignore cell filled-p))
     (multiple-value-bind (row col) (cell-row-col neighbour)
       (declare (ignore row))
       (or (< col 0) (>= col (width field)))))))

(defun members-touching-floor-count (pos field)
  (members-matches-counter
   pos field
   (lambda (neighbour cell filled-p)
     (declare (ignore cell filled-p))
     (multiple-value-bind (row col) (cell-row-col neighbour)
       (declare (ignore col))
       (>= row (height field))))))

(defmethod estimate ((solver hedonistic-solver) (field hextris-map) (checking-pos unit-on-map) &key translated-pos)
  (let ((locked-field (unit-lock (unit-on-map-unit checking-pos) (unit-on-map-coord checking-pos) field))
        (translated-pos (or translated-pos (place-on-map (unit-on-map-unit checking-pos) (unit-on-map-coord checking-pos) field))))
    (+ (* (count-rows-fill locked-field) (row-fill-factor solver))
       (* (find-out-sum-of-heights locked-field) (sum-of-heights-factor solver))
       (* (find-out-burned-rows-count locked-field) (row-burn-factor solver))
       (* (find-out-blockades-count locked-field) (blockade-factor solver))
       (* (members-touching-something-count translated-pos field) (touching-something-factor solver))
       (* (members-touching-wall-count translated-pos field) (touching-wall-factor solver))
       (* (members-touching-floor-count translated-pos field) (touching-floor-factor solver)))))

  
