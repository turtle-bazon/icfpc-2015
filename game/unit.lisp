
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
    (when (every (lambda (c) (multiple-value-bind (cell filled-p) (map-cell field c) (declare (ignore cell)) (not filled-p)))
                 translated)
      (make-instance 'unit :members translated))))

;;;; TODO: to be fixed

;; (defun unit-rotate* (unit transform)
;;   (bind ((pivot-row (cell-row (pivot unit)))
;;          (pivot-col (cell-col (pivot unit)))
;;          ;; Copy cells, normalize coordinates by pivot and update cubic coordinates
;;          (cells (mapcar #'(lambda (cell)
;;                             (cell-update-cube
;;                              (make-cell :row (- (cell-row cell) pivot-row)
;;                                         :col (- (cell-col cell) pivot-col))))
;;                         (members unit))))
;;     ;; Do rotation, move to offset coordinates and denormalize
;;     (iter (for cell in cells)
;;           (funcall transform cell)
;;           (cell-update-offset cell)
;;           (setf (cell-row cell) (+ pivot-row (cell-row cell))
;;                 (cell-col cell) (+ pivot-row (cell-col cell))))
;;     (make-instance 'unit
;;                    :pivot (pivot unit)
;;                    :members cells)))

;; (defun unit-rotate-clockwise-op (cell)
;;   (bind ((x (cell-cube-x cell))
;;          (y (cell-cube-y cell))
;;          (z (cell-cube-z cell)))
;;     (setf (cell-cube-x cell) (- z)
;;           (cell-cube-y cell) (- x)
;;           (cell-cube-z cell) (- y))))

;; (defun unit-rotate-counter-clockwise-op (cell)
;;   (bind ((x (cell-cube-x cell))
;;          (y (cell-cube-y cell))
;;          (z (cell-cube-z cell)))
;;     (setf (cell-cube-x cell) (- y)
;;           (cell-cube-y cell) (- z)
;;           (cell-cube-z cell) (- x))))

;; (defmethod unit-move ((map hextris-map) (unit unit) (direction (eql :w)))
;;   )

;; (defmethod unit-move ((map hextris-map) (unit unit) (direction (eql :e)))
;;   )

;; (defmethod unit-move ((map hextris-map) (unit unit) (direction (eql :sw)))
;;   )

;; (defmethod unit-move ((map hextris-map) (unit unit) (direction (eql :se)))
;;   )

;; (defmethod unit-rotate ((map hextris-map) (unit unit) (direction (eql :rcw)))
;;   (bind ((rotated-unit (unit-rotate* unit #'unit-rotate-clockwise-op)))
;;     (iter (for cell in (members rotated-unit))
;;           (unless (map-cell-free-p map cell)
;;             (return nil))
;;           (finally (return cell)))))

;; (defmethod unit-rotate ((map hextris-map) (unit unit) (direction (eql :rcc)))
;;   (bind ((rotated-unit (unit-rotate* unit #'unit-rotate-counter-clockwise-op)))
;;     (iter (for cell in (members rotated-unit))
;;           (unless (map-cell-free-p map cell)
;;             (return nil))
;;           (finally (return cell)))))

;; (defmethod unit-lock ((obj unit) (field hextris-map))
;;   (let ((field-copy (clone-map field)))
;;     (iter (for cell in (members obj))
;;           (setf (map-cell field-copy cell) t))
;;     field-copy))
