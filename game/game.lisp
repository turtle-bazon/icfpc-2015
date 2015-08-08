
(in-package :hextris)

(defclass game ()
  ((problem-id :initarg :problem-id :reader problem-id)
   (source-length :initarg :source-length :reader source-length)
   (map :initarg :map :accessor game-map)
   (units :initarg :units :reader units)
   (seeds :initarg :seeds :reader seeds)))

(defmethod game-loop ((world game))
  (iter (for seed in (seeds world)) ;; play one game for each seed given
        (for rng = (make-rng seed))
        (iter (repeat (source-length world)) ;; spawn as many units as given in source-length

              )))
              
(defun make-next-unit (game rng)
  (bind ((next-number (funcall rng))
         (next-unit-number (mod next-number (length (units game)))))
    (nth next-unit-number (units game))))
