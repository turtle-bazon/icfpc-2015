
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
              (for best-choice =
                   (iter (with installed-unit = (make-unit-on-map :unit (unit-on-map-unit initial-unit)
                                                                  :coord (make-cell-row-col row col)))
                         (repeat 6) ;;; rotate six times
                         (for translated = (place-on-map (unit-on-map-unit installed-unit)
                                                         (unit-on-map-coord installed-unit)
                                                         field))
                         (when translated
                           (multiple-value-bind (reachable-p move-script)
                               (run-a-star field initial-unit installed-unit)
                             (when reachable-p
                               (for lowest-row = (iter (for m in (members (unit-on-map-unit installed-unit)))
                                                       (multiple-value-bind (row col) (cell-row-col m)
                                                         (declare (ignore col))
                                                         (minimizing row))))
                               (finding (list installed-unit move-script)
                                        minimizing (count-free-cells lowest-row
                                                                     (unit-lock (unit-on-map-unit installed-unit)
                                                                                (unit-on-map-coord installed-unit)
                                                                                field))))))
                         (setf installed-unit (cdr (move-unit :rcw installed-unit field)))))
              (when best-choice
                (return-from locate-target (values-list best-choice))))))
