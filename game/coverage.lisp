
(in-package :hextris)

(defmethod translate-members ((obj unit-on-map) (field hextris-map))
  (let* ((pivot-row (cell-row (unit-on-map-coord obj)))
         (pivot-col (cell-col (unit-on-map-coord obj)))
         (translated (mapcar (lambda (c) (make-cell :row (+ (cell-row c) pivot-row) :col (+ (cell-col c) pivot-col)))
                             (members (unit-on-map-unit obj)))))
    (when (every (lambda (c) (multiple-value-bind (found-cell filled-p) (map-cell field c) (and found-cell (not filled-p)))) translated)
      translated)))

(defmethod count-holes ((field hextris-map) row)
  (iter (with holes-count = 0)
        (with switch = :filled)
        (for col from 0 below (width field))
        (multiple-value-bind (cell filled-p)
            (map-cell field (make-cell :row row :col col))
          (declare (ignore cell))
          (if filled-p
              (ecase switch
                (:hole (setf switch :filled))
                (:filled))
              (ecase switch
                (:filled (incf holes-count) (setf switch :hole))
                (:hole))))
        (finally (return holes-count))))
  
(defmethod locate-target ((field hextris-map) (sample-unit unit))
  (iter (for row from (1- (height field)) downto 0)
        (for best-position = 
             (iter outer-loop
                   (for col from 0 below (width field))
                   (iter (with installed-unit = (make-unit-on-map :unit sample-unit
                                                                  :coord (make-cell :row row :col col)))
                         (repeat 6) ;;; rotate six times
                         (for translated-members = (translate-members installed-unit field))
                         (when translated-members
                           (in outer-loop
                               (finding installed-unit minimizing (count-holes (unit-lock (unit-on-map-unit installed-unit) field)
                                                                               (1- (height field))))))
                         (setf installed-unit (cdr (move-unit :rotate-cw installed-unit field))))))
        (when best-position
          (return-from locate-target best-position))))
