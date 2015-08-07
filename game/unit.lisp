
(in-package :hextris)

(deftestsuite unit-tests () ())

(defclass unit ()
  ((members :initarg :members :accessor members)))

(defmethod place-on-map ((obj unit) (coord cell) (field hextris-map))
  (let ((translated (mapcar (lambda (c)
                              (make-cell :cube-x (+ (cell-cube-x c) (cell-cube-x coord))
                                         :cube-y (+ (cell-cube-y c) (cell-cube-y coord))
                                         :cube-z (+ (cell-cube-z c) (cell-cube-z coord))))
                            (members obj))))
    (when (every (lambda (c) (multiple-value-bind (cell filled-p) (map-cell field c) (and cell (not filled-p))))
                 translated)
      (make-instance 'unit :members translated))))

;;;; TODO: to be fixed
(defun unit-rotate-clockwise-op (cell)
  (bind ((x (cell-cube-x cell))
         (y (cell-cube-y cell))
         (z (cell-cube-z cell)))
    (setf (cell-cube-x cell) (- z)
          (cell-cube-y cell) (- x)
          (cell-cube-z cell) (- y))))

(defun unit-rotate-counter-clockwise-op (cell)
  (bind ((x (cell-cube-x cell))
         (y (cell-cube-y cell))
         (z (cell-cube-z cell)))
    (setf (cell-cube-x cell) (- y)
          (cell-cube-y cell) (- z)
          (cell-cube-z cell) (- x))))

(defun unit-rotate* (unit transform)
  (make-instance 'unit
                 :members (iter
                            ;; Copy cells
                            (for cell in (mapcar #'copy-cell (members unit)))
                            ;; Transform coordinates
                            (collect (funcall transform cell)))))

(defun unit-rotate (unit direction)
  (ecase direction
    (:rcw (unit-rotate* unit #'unit-rotate-clockwise-op))
    (:rcc (unit-rotate* unit #'unit-rotate-counter-clockwise-op))))

;; (defmethod unit-move ((map hextris-map) (unit unit) (direction (eql :w)))
;;   )

;; (defmethod unit-move ((map hextris-map) (unit unit) (direction (eql :e)))
;;   )

;; (defmethod unit-move ((map hextris-map) (unit unit) (direction (eql :sw)))
;;   )

;; (defmethod unit-move ((map hextris-map) (unit unit) (direction (eql :se)))
;;   )

(defmethod unit-lock ((obj unit) (coord cell) (field hextris-map))
  (let ((translated-unit (place-on-map obj coord field))
        (field-copy (clone-map field)))
    (iter (for cell in (members translated-unit))
          (setf (map-cell field-copy cell) t))
    field-copy))

(defmethod debug-draw ((obj unit) (coord cell) (field hextris-map))
  (iter (with translated-unit = (place-on-map obj coord field))
        (for row from 0 below (height field))
        (when (oddp row)
          (format t " "))
        (iter (for col from 0 below (width field))
              (multiple-value-bind (cell filled-p) (map-cell field (make-cell-row-col row col))
                (format t "~a " (if (find cell (members translated-unit) :test #'cell=)
                                    "o"
                                    (if filled-p "x" ".")))))
        (format t "~%")))
