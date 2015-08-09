
(in-package :hextris)

(defclass hedonistic-solver (solver)
  ())

(defparameter *sum-of-heights-factor* -0.03)
(defparameter *row-burn-factor* 8.0)
(defparameter *blockade-factor* -3.5)
(defparameter *touching-something-factor* 3.0)
(defparameter *touching-wall-factor* 2.5)
(defparameter *touching-floor-factor* 5.0)

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

(defun find-out-blockades-count (field)
  (iter (with blockades-count = 0)
        (for col from 0 below (width field))
        (iter (with blocked-p = nil)
              (for row from 0 below (height field))
              (if (map-cell-free-p* field row col)
                  (setf blocked-p t)
                  (when blocked-p
                    (incf blockades-count))))
        (finally (return blockades-count))))

(defun members-touching-something-count (pos field)
  ;; TODO
  0)

(defun members-touching-wall-count (pos field)
  ;; TODO
  0)

(defun members-touching-floor-count (pos field)
  ;; TODO
  0)

(defmethod estimate ((solver hedonistic-solver) (field hextris-map) (checking-pos unit-on-map))
  (let ((locked-field (unit-lock (unit-on-map-unit checking-pos) (unit-on-map-coord checking-pos) field)))
    (+ (* (find-out-sum-of-heights locked-field) *sum-of-heights-factor*)
       (* (find-out-burned-rows-count locked-field) *row-burn-factor*)
       (* (find-out-blockades-count locked-field) *blockade-factor*)
       (* (members-touching-something-count checking-pos field) *touching-something-factor*)
       (* (members-touching-wall-count checking-pos field) *touching-wall-factor*)
       (* (members-touching-floor-count checking-pos field) *touching-floor-factor*))))

  
