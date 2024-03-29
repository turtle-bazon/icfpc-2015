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

(defmethod debug-draw ((field hextris-map) &key current-position final-position)
  (iter (with translated-current =
              (when current-position
                (place-on-map (unit-on-map-unit current-position) (unit-on-map-coord current-position) field)))
        (with translated-final =
              (when final-position
                (place-on-map (unit-on-map-unit final-position) (unit-on-map-coord final-position) field)))
        (for row from 0 below (height field))
        (when (oddp row)
          (format t " "))
        (iter (for col from 0 below (width field))
              (multiple-value-bind (cell filled-p) (map-cell field (make-cell-row-col row col))
                (format t "~a " (if (and current-position (cell= cell (unit-on-map-coord current-position)))
                                    "*"
                                    (if (and translated-current (find cell (members translated-current) :test #'cell=))
                                        "o"
                                        (if (and translated-final (find cell (members translated-final) :test #'cell=))
                                            "#"
                                            (if filled-p "x" ".")))))))
        (format t "~%")
        (finally
         (when (and (boundp '*info-printer*) (functionp *info-printer*))
           (funcall *info-printer* field
                    :current-position current-position
                    :final-position final-position
                    :translated-current translated-current
                    :translated-final translated-final)))))

(defmethod debug-draw-unit ((obj unit))
  (multiple-value-bind (width height) (unit-dimensions obj)
    (let ((field (make-instance 'hextris-map :width (1+ (* width 2)) :height (1+ (* height 2)))))
      (debug-draw field :current-position (make-unit-on-map :unit obj :coord (make-cell-row-col height width))))))

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
                        (oddp l-row)) l)
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

(defmethod unit-dimensions ((obj unit))
  (iter (for mem in (members obj))
        (for (values row col) = (cell-row-col mem))
        (minimizing row into min-row)
        (maximizing row into max-row)
        (finally (return (values (unit-width obj) (1+ (- max-row min-row)))))))

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

(defmethod unit-initial-position-v2 ((obj unit) (field hextris-map))
  (multiple-value-bind (top-row top-col)
      (cell-row-col (unit-top-most obj))
    (declare (ignore top-col))
    (iter (with best = nil)
          (with best-dist = nil)
          (for col from 0 below (width field))
          (for row = (- 0 top-row))
          (for translated = (place-on-map obj (make-cell-row-col row col) field))
          (when translated
            (for (values min-left-sq-dist min-right-sq-dist) =
                 (iter (for cell in (members translated))
                       (for (values cell-row cell-col) = (cell-row-col cell))
                       (for left-limit = (make-cell-row-col cell-row 0))
                       (for right-limit = (make-cell-row-col cell-row (1- (width field))))
                       (for left-sq-dist = (sq-dist cell left-limit))
                       (for right-sq-dist = (sq-dist cell right-limit))
                       (minimizing left-sq-dist into min-left-sq-dist)
                       (minimizing right-sq-dist into min-right-sq-dist)
                       (finally (return (values min-left-sq-dist min-right-sq-dist)))))
            (when (or (not best)
                      (< (abs (- min-right-sq-dist min-left-sq-dist))
                         (abs (- (cdr best-dist) (car best-dist))))
                      (and (= (abs (- min-right-sq-dist min-left-sq-dist))
                              (abs (- (cdr best-dist) (car best-dist))))
                           (< min-left-sq-dist min-right-sq-dist)))
              (setf best (list row col))
              (setf best-dist (cons min-left-sq-dist min-right-sq-dist))))
          (finally (return (when best (make-cell-row-col (first best) (second best))))))))

(defmethod unit-initial-position-v3 ((obj unit) (field hextris-map))
  (bind ((left-most (unit-left-most obj))
         (right-most (unit-right-most obj))
         (top-most (unit-top-most obj))
         ((:values _ col) (cell-row-col left-most))
         ((:values row _) (cell-row-col top-most))
         (unit-width (unit-width obj))
         (top-left-local-cell (make-cell-row-col row col))
         (top-left-global-col (floor (/ (- (width field) unit-width) 2)))
         (top-left-global-cell (make-cell-row-col 0 top-left-global-col))
         (pivot-candidate (make-cell :cube-x (- (cell-cube-x top-left-global-cell)
                                                (cell-cube-x top-left-local-cell))
                                     :cube-y (- (cell-cube-y top-left-global-cell)
                                                (cell-cube-y top-left-local-cell))
                                     :cube-z (- (cell-cube-z top-left-global-cell)
                                                (cell-cube-z top-left-local-cell))))
         (left-most-candidate (make-cell :cube-x (+ (cell-cube-x left-most)
                                                    (cell-cube-x pivot-candidate))
                                         :cube-y (+ (cell-cube-y left-most)
                                                    (cell-cube-y pivot-candidate))
                                         :cube-z (+ (cell-cube-z left-most)
                                                    (cell-cube-z pivot-candidate))))
         (right-most-candidate (make-cell :cube-x (+ (cell-cube-x right-most)
                                                     (cell-cube-x pivot-candidate))
                                          :cube-y (+ (cell-cube-y right-most)
                                                     (cell-cube-y pivot-candidate))
                                          :cube-z (+ (cell-cube-z right-most)
                                                     (cell-cube-z pivot-candidate))))
         ((:values _ left-most-col) (cell-row-col left-most-candidate))
         ((:values _ right-most-col) (cell-row-col right-most-candidate)))

    ;; Now need to check left-right margins
    (when (< (1+ left-most-col) (- (width field) right-most-col 1))
      (cell-move* pivot-candidate :e))

    pivot-candidate))

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

