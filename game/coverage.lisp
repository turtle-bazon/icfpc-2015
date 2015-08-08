
(in-package :hextris)

(defmethod count-holes ((field hextris-map) row)
  (iter (with holes-count = 0)
        (with switch = :filled)
        (for col from 0 below (width field))
        (multiple-value-bind (cell filled-p)
            (map-cell field (make-cell-row-col row col))
          (declare (ignore cell))
          (if filled-p
              (ecase switch
                (:hole (setf switch :filled))
                (:filled))
              (ecase switch
                (:filled (incf holes-count) (setf switch :hole))
                (:hole))))
        (finally (return holes-count))))

(defmethod count-free-cells (row (field hextris-map))
  (iter (for col from 0 below (width field))
        (for cell = (make-cell-row-col row col))
        (counting (multiple-value-bind (cell filled-p) (map-cell field cell)
                    (and cell (not filled-p))))))
  
(defmethod locate-target ((field hextris-map) (sample-unit unit))
  (iter (for row from (1- (height field)) downto 0)
        (iter (for col from 0 below (width field))
              (for best-position =
                   (iter (with installed-unit = (make-unit-on-map :unit sample-unit :coord (make-cell-row-col row col)))
                         (repeat 6) ;;; rotate six times
                         (for translated = (place-on-map (unit-on-map-unit installed-unit)
                                                         (unit-on-map-coord installed-unit)
                                                         field))
                         (when translated
                           (for lowest-row = (iter (for m in (members (unit-on-map-unit installed-unit)))
                                                   (multiple-value-bind (row col) (cell-row-col m)
                                                     (declare (ignore col))
                                                     (minimizing row))))
                           (finding installed-unit
                                    minimizing (count-free-cells lowest-row
                                                                 (unit-lock (unit-on-map-unit installed-unit)
                                                                            (unit-on-map-coord installed-unit)
                                                                            field))))
                         (setf installed-unit (cdr (move-unit :rcw installed-unit field)))))
              (when best-position
                (return-from locate-target best-position)))))
