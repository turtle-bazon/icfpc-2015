
(in-package :hextris)

(defclass game ()
  ((problem-id :initarg :problem-id :reader problem-id)
   (source-length :initarg :source-length :reader source-length)
   (map :initarg :map :accessor game-map)
   (units :initarg :units :reader units)
   (seeds :initarg :seeds :reader seeds)))

(defmethod game-loop ((world game)
                      &optional &key time-limit memory-limit number-cores phrases)
  (declare (optimize (debug 3)))
  (iter (for seed in (seeds world)) ;; play one game for each seed given
        (for rng = (make-rng seed))
        (for game-script =
             (iter (with current-map = (game-map world))
                   (repeat (source-length world)) ;; spawn as many units as given in source-length
                   (for next-unit = (make-next-unit world rng))
                   (for init-position = (unit-initial-position next-unit current-map))
                   (multiple-value-bind (final-position moves-script)
                       (locate-target current-map (make-unit-on-map :unit next-unit :coord init-position))
                     (unless final-position ;; probably this is a stop condition?
                       (terminate))
                     (setf current-map ;; freeze fallen unit at it's final position
                           (unit-lock (unit-on-map-unit final-position)
                                      (unit-on-map-coord final-position)
                                      current-map))
                     (setf current-map ;; maybe burn some rows if any
                           (map-burn-lines-v2 current-map))
                     (for moves-script+freeze = (append moves-script (list :sw)))
                     (appending moves-script+freeze))))
        (collecting (list :game world :seed seed :script game-script))))

(defun make-next-unit (game rng)
  (bind ((next-number (funcall rng))
         (next-unit-number (mod next-number (length (units game)))))
    (nth next-unit-number (units game))))
