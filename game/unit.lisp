(in-package :hextris)

;; (deftestsuite unit-tests () ())

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
(defun cell-rotate-clockwise-op (cell)
  (bind ((x (cell-cube-x cell))
         (y (cell-cube-y cell))
         (z (cell-cube-z cell)))
    (make-cell :cube-x (- z)
               :cube-y (- x)
               :cube-z (- y))))

(defun cell-rotate-counter-clockwise-op (cell)
  (bind ((x (cell-cube-x cell))
         (y (cell-cube-y cell))
         (z (cell-cube-z cell)))
    (make-cell :cube-x (- y)
               :cube-y (- z)
               :cube-z (- x))))

(defun unit-rotate* (unit transform)
  (make-instance 'unit :members (mapcar transform (members unit))))

(defun unit-rotate (unit direction)
  (ecase direction
    (:rcw (unit-rotate* unit #'cell-rotate-clockwise-op))
    (:rcc (unit-rotate* unit #'cell-rotate-counter-clockwise-op))))

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

(defmethod unit-left-most ((obj unit))
  (reduce #'(lambda (l r)
              (bind (((:values l-row l-col) (cell-row-col l))
                     ((:values _ r-col) (cell-row-col r)))
                (cond
                  ((< l-col r-col) l)
                  ((and (= l-col r-col)
                        (evenp l-row)) l)
                  (t r))))
          (members obj)))

(defmethod unit-right-most ((obj unit))
  (reduce #'(lambda (l r)
              (bind (((:values l-row l-col) (cell-row-col l))
                     ((:values _ r-col) (cell-row-col r)))
                (cond
                  ((> l-col r-col) l)
                  ((and (= l-col r-col)
                        (evenp l-row)) l)
                  (t r))))
          (members obj)))

(defmethod unit-top-most ((obj unit))
  (reduce #'(lambda (l r)
              (bind (((:values l-row _) (cell-row-col l))
                     ((:values r-row _) (cell-row-col r)))
                (cond
                  ((< l-row r-row) l)
                  (t r))))
          (members obj)))

(defmethod unit-width ((obj unit))
  (bind (((:values _ l-col) (cell-row-col (unit-left-most obj)))
         ((:values _ r-col) (cell-row-col (unit-right-most obj))))
    (1+ (abs (- r-col l-col)))))

(defmethod unit-initial-position ((obj unit) (field hextris-map))
  (bind ((left-most (unit-left-most obj))
         (top-most (unit-top-most obj))
         ((:values _ col) (cell-row-col left-most))
         ((:values row _) (cell-row-col top-most))
         (unit-width (unit-width obj))
         (top-left-local-cell (make-cell-row-col row col))
         (top-left-global-col (floor (/ (- (width field) unit-width) 2)))
         (top-left-global-cell (make-cell-row-col 0 top-left-global-col)))
    (make-cell :cube-x (- (cell-cube-x top-left-global-cell)
                          (cell-cube-x top-left-local-cell))
               :cube-y (- (cell-cube-y top-left-global-cell)
                          (cell-cube-y top-left-local-cell))
               :cube-z (- (cell-cube-z top-left-global-cell)
                          (cell-cube-z top-left-local-cell)))))

(defmethod unit-position-possible-p ((obj unit) (position cell) (field hextris-map))
  (iter
    (for base-cell in (members obj))
    (for real-cell = (make-cell :cube-x (+ (cell-cube-x base-cell)
                                           (cell-cube-x position))
                                :cube-y (+ (cell-cube-y base-cell)
                                           (cell-cube-y position))
                                :cube-z (+ (cell-cube-z base-cell)
                                           (cell-cube-z position))))
    (always (map-cell-free-p** field real-cell))))

