
(in-package :hextris)

(defclass game ()
  ((problem-id :initarg :problem-id :reader problem-id)
   (source-length :initarg :source-length :reader source-length)
   (map :initarg :map :accessor game-map)
   (units :initarg :units :reader units)
   (seeds :initarg :seeds :reader seeds)))

(defmethod single-game-loop ((world game) seed &optional &key record-film time-limit memory-limit number-cores phrases)
  (declare (optimize (debug 3))
           (ignore time-limit memory-limit number-cores phrases))
  (let ((rng (make-rng seed)))
    (multiple-value-bind (game-script film)
        (iter (with current-map = (game-map world))
              (repeat (source-length world)) ;; spawn as many units as given in source-length
              (for next-unit = (make-next-unit world rng))
              (for init-position = (unit-initial-position-v3 next-unit current-map))
              (unless init-position
                (terminate))
              (for init-position-on-map = (make-unit-on-map :unit next-unit :coord init-position))
              (unless (unit-position-possible-p next-unit init-position current-map)
                (terminate))
              (multiple-value-bind (final-position moves-script)
                  (locate-target current-map init-position-on-map)
                (unless final-position ;; probably this is a stop condition?
                  (terminate))
                ;;; record film
                (for next-unit-frames =
                     (when record-film
                       (iter (with board-filled = (iter outer
                                                        (for row from 0 below (height current-map))
                                                        (iter (for col from 0 below (width current-map))
                                                              (multiple-value-bind (cell filled-p)
                                                                  (map-cell current-map (make-cell-row-col row col))
                                                                (when (and cell filled-p)
                                                                  (in outer (collect (list (cons :y row) (cons :x col)))))))))
                             (with actor = init-position-on-map)
                             (for move in moves-script)
                             (for translated = (place-on-map (unit-on-map-unit actor)
                                                             (unit-on-map-coord actor)
                                                             current-map))
                             (assert translated)
                             (for moved-actor = (move-unit move actor current-map))

;;; vizualize for the weaks
                             (format t "~a~&" move)
                             (debug-draw current-map :current-position actor :final-position final-position)
                             (format t "~%")
                             (debug-draw current-map :current-position moved-actor)
                             
                             (format t "---------------------~%")
                             (break)
                             (setf actor moved-actor)
                             (collect (list (cons :filled (coerce board-filled 'vector))
                                            (cons :unit-members (coerce (iter (for cell in (members translated))
                                                                              (for (values row col) = (cell-row-col cell))
                                                                              (collect (list (cons :y row) (cons :x col))))
                                                                        'vector)))))))

;;; generate freeze move
                (for freeze-move = (gen-freeze-move current-map final-position))
;;; update map
                (setf current-map ;; freeze fallen unit at it's final position
                      (unit-lock (unit-on-map-unit final-position)
                                 (unit-on-map-coord final-position)
                                 current-map))
                (setf current-map ;; maybe burn some rows if any
                      (map-burn-lines-v2 current-map))
                (for moves-script+freeze = (append moves-script (list freeze-move)))
                (appending moves-script+freeze into script)
                (appending next-unit-frames into frames)
                (finally (return (values script frames)))))
      (list :game world :seed seed :script game-script :film film))))

(defmethod game-loop ((world game) &optional &key record-film time-limit memory-limit number-cores phrases)
  (iter (for seed in (seeds world))
        (for rng = (make-rng seed))
        (for (values game-script film) =
             (collect
                 (single-game-loop world seed
                                   :record-film record-film
                                   :time-limit time-limit
                                   :memory-limit memory-limit
                                   :number-cores number-cores
                                   :phrases phrases)))))

(defun make-next-unit (game rng)
  (bind ((next-number (funcall rng))
         (next-unit-number (mod next-number (length (units game)))))
    (nth next-unit-number (units game))))
