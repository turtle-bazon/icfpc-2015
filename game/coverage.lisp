
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

(defmethod locate-target ((field hextris-map) (initial-unit unit-on-map))
  (iter (for row from (1- (height field)) downto 0)
        (iter (for col from 0 below (width field))
              (for variants =
                   (iter (with installed-unit = (make-unit-on-map :unit (unit-on-map-unit initial-unit)
                                                                  :coord (make-cell-row-col row col)))
                         (repeat 6) ;;; rotate six times
                         (for translated = (place-on-map (unit-on-map-unit installed-unit)
                                                         (unit-on-map-coord installed-unit)
                                                         field))
                         (when (and translated (gen-freeze-move field installed-unit))
                           (for lowest-row = (iter (for m in (members (unit-on-map-unit installed-unit)))
                                                   (multiple-value-bind (row col) (cell-row-col m)
                                                     (declare (ignore col))
                                                     (minimizing row))))
                           (for total-free-cells = (count-free-cells lowest-row
                                                                     (unit-lock (unit-on-map-unit installed-unit)
                                                                                (unit-on-map-coord installed-unit)
                                                                                field)))
                           (collect (cons installed-unit total-free-cells)))
                         (setf installed-unit (move-unit :rcw installed-unit field))))
              (iter (for (candidate-unit . free-cells) in (sort variants #'< :key #'cdr))                    
                    (multiple-value-bind (reachable-p move-script)
                        (run-a-star field initial-unit candidate-unit)
                      (when reachable-p
                        (return-from locate-target (values candidate-unit move-script))))))))
