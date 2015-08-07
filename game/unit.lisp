
(in-package :hextris)

(deftestsuite unit-tests () ())

(defclass unit ()
  ((members :initarg :members :accessor members)
   (pivot :initarg :pivot :type cell :accessor pivot)))

(defun unit-rotate* (unit transform)
  (bind ((pivot-row (cell-row (pivot unit)))
         (pivot-col (cell-col (pivot unit)))
         ;; Copy cells, normalize coordinates by pivot and update cubic coordinates
         (cells (mapcar #'(lambda (cell)
                            (cell-update-cube
                             (make-cell :row (- (cell-row cell) pivot-row)
                                        :col (- (cell-col cell) pivot-col))))
                        (members unit))))
    ;; Do rotation, move to offset coordinates and denormalize
    (iter (for cell in cells)
          (funcall transform cell)
          (cell-update-offset cell)
          (setf (cell-row cell) (+ pivot-row (cell-row cell))
                (cell-col cell) (+ pivot-row (cell-col cell))))
    (make-instance 'unit
                   :pivot (pivot unit)
                   :members cells)))

(defun rotate-clockwise (cell)
  (bind ((x (cell-cube-x cell))
         (y (cell-cube-y cell))
         (z (cell-cube-z cell)))
    (setf (cell-cube-x cell) (- z)
          (cell-cube-y cell) (- x)
          (cell-cube-z cell) (- y))))

(defun rotate-counter-clockwise (cell)
  (bind ((x (cell-cube-x cell))
         (y (cell-cube-y cell))
         (z (cell-cube-z cell)))
    (setf (cell-cube-x cell) (- y)
          (cell-cube-y cell) (- z)
          (cell-cube-z cell) (- x))))

(defmethod unit-move ((map hextris-map) (unit unit) (direction (eql :w)))
  )

(defmethod unit-move ((map hextris-map) (unit unit) (direction (eql :e)))
  )

(defmethod unit-move ((map hextris-map) (unit unit) (direction (eql :sw)))
  )

(defmethod unit-move ((map hextris-map) (unit unit) (direction (eql :se)))
  )

(defmethod unit-rotate ((map hextris-map) (unit unit) (direction (eql :rcw)))
  (bind ((rotated-unit (unit-rotate* unit #'rotate-clockwise)))
    (iter (for cell in (members rotated-unit))
          (unless (map-cell-free-p map cell)
            (return nil))
          (finally (return cell)))))

(defmethod unit-rotate ((map hextris-map) (unit unit) (direction (eql :rcc)))
  (bind ((rotated-unit (unit-rotate* unit #'rotate-counter-clockwise)))
    (iter (for cell in (members rotated-unit))
          (unless (map-cell-free-p map cell)
            (return nil))
          (finally (return cell)))))

