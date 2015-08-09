
(in-package :hextris)

(defclass hedonistic-solver (solver)
  ())

(defparameter *sum-of-heights-factor* -0.03)
(defparameter *row-burn-factor* 80)
(defparameter *blockade-factor* -3.5)
(defparameter *touching-something-factor* 3.0)
(defparameter *touching-wall-factor* 2.5)
(defparameter *touching-floor-factor* 5.0)
(defparameter *row-fill-factor* 1)

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
        (for row-estimate = (* (/ (+ row-sum row)
                                  (expt 2 (- (height field) row 1)))
                               (if (= row-count (width field))
                                   row
                                   1)))
        (format t "W: ~a, H: ~a, R: ~a, RS: ~a, RE: ~f~&"
                (width field) (height field) row row-sum row-estimate)
        (sum row-estimate)))

. . . . . . . . . . 
. . . . . . . . . . 
x . x . x . x . x . 
. . . . . . . . . . 
. . . . . . . . . . 
. . . x . . . . . x 
x x x x . . . . . . 
W: 10, H: 7, R: 5, RS: 2, RE: 3.5
W: 10, H: 7, R: 6, RS: 10, RE: 96.0
Estimate: 101.34375

. . . . . . . . . . 
. . . . . . . . . . 
x . x . x . x . x . 
. . . . . . . . . . 
. . . . . . . . . . 
. . . . . . . . . x 
x x x x x . . . . . 
W: 10, H: 7, R: 5, RS: 1, RE: 3.0
W: 10, H: 7, R: 6, RS: 15, RE: 21.0
Estimate: 25.84375

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
    burned-rows-count))

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

(defmethod estimate ((solver hedonistic-solver) (field hextris-map) (checking-pos unit-on-map))
  (let ((locked-field (unit-lock (unit-on-map-unit checking-pos) (unit-on-map-coord checking-pos) field))
        (translated-pos (place-on-map (unit-on-map-unit checking-pos) (unit-on-map-coord checking-pos) field)))
    (format t "Calculate estimate position:~&")
    (debug-draw locked-field)
    (format t "Estimate: ~f~&" (count-rows-fill locked-field))
    ;; (break)
    #+nil(+ (* (find-out-sum-of-heights locked-field) *sum-of-heights-factor*)
       (* (find-out-burned-rows-count locked-field) *row-burn-factor*)
       (* (find-out-blockades-count locked-field) *blockade-factor*)
       (* (members-touching-something-count translated-pos field) *touching-something-factor*)
       (* (members-touching-wall-count translated-pos field) *touching-wall-factor*)
       (* (members-touching-floor-count translated-pos field) *touching-floor-factor*))
    (count-rows-fill locked-field)))

  
