
(in-package :hextris)

(defmethod locate-target ((field hextris-map) (initial-unit unit-on-map) &key (solver (make-instance 'hedonistic-solver)))
  (iter outermost
        (for row from 0 below (height field))
        (iter (for col from 0 below (width field))
              (for installed-unit = (make-unit-on-map :unit (unit-on-map-unit initial-unit)
                                                      :coord (make-cell-row-col row col)))
              (iter (repeat 6) ;;; rotate six times
                    (for translated = (place-on-map (unit-on-map-unit installed-unit)
                                                    (unit-on-map-coord installed-unit)
                                                    field))
                    (when (and translated (gen-freeze-move field installed-unit))
                      (in outermost (collect (cons installed-unit
                                                   (estimate solver field installed-unit :translated-pos translated))
                                      into candidates)))
                    (setf installed-unit (move-unit :rcw installed-unit field))))
        (finally
         (iter (for (candidate-unit . estimate-score) in (sort candidates #'> :key #'cdr))
               (multiple-value-bind (reachable-p move-script)
                   (run-a-star field initial-unit candidate-unit)
                 (when reachable-p
                   (return-from locate-target (values candidate-unit move-script))))))))

(defun debug-locate-visualize (filename)
  (let ((*info-printer* (lambda (field &key current-position final-position &allow-other-keys)
                          (if final-position
                              (format t " ;; FINAL estimate = ~a~%"
                                      (estimate (make-instance 'hedonistic-solver) field final-position))
                              (format t " ;; CURRENT estimate = ~a~%"
                                      (estimate (make-instance 'hedonistic-solver) field current-position))))))
    (game-loop (parse-input-file filename) :record-film t)))
