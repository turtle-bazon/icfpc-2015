
(in-package :hextris)

(defclass game ()
  ((problem-id :initarg :problem-id :reader problem-id)
   (source-length :initarg :source-length :reader source-length)
   (map :initarg :map :accessor game-map)
   (units :initarg :units :reader units)
   (seeds :initarg :seeds :reader seeds)))

(defmethod single-game-loop ((world game) seed &optional &key record-film time-limit memory-limit number-cores phrases)
  (declare (optimize (debug 3))
           (ignore time-limit memory-limit number-cores))
  (let ((rng (make-rng seed))
        (*power-phrases* phrases)
        (power-phrases-alist (power-phrases-alist *power-phrases*)))
    (multiple-value-bind (game-script film move-score power-score)
        (iter (with current-map = (game-map world))
              (with move-score = 0)
              (with power-score = 0)
              (with power-phrases-used = (make-hash-table :test #'equal))
              (for ls = 0)
              (for ls-old initially 0 then ls)
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
                             (format t "move score: ~a power score: ~a~%SCORE: ~a~%" move-score power-score (+ move-score
                                                                                                               power-score))
                             (break)
                             (setf actor moved-actor)
                             (collect (list (cons :filled (coerce board-filled 'vector))
                                            (cons :unit-members (coerce (iter (for cell in (members translated))
                                                                              (for (values row col) = (cell-row-col cell))
                                                                              (collect (list (cons :y row) (cons :x col))))
                                                                        'vector)))))))

;;; generate freeze move
                (for freeze-move = (gen-freeze-move current-map final-position :script (append script moves-script)))
;;; record script                
                (for moves-script+freeze = (append moves-script (list freeze-move)))
                (appending moves-script+freeze into script)
;;; update map
                (setf current-map ;; freeze fallen unit at it's final position
                      (unit-lock (unit-on-map-unit final-position)
                                 (unit-on-map-coord final-position)
                                 current-map))
                (multiple-value-bind (map rows-burned)
                    (map-burn-lines-v2 current-map) ;; maybe burn some rows if any
                    (setf current-map map
                          ls rows-burned))
;; calculate move score
;; move_score = points + line_bonus
;;   where
;;   points = size + 100 * (1 + ls) * ls / 2
;;   line_bonus  = if ls_old > 1
;;                 then floor ((ls_old - 1) * points / 10)
;;                 else 0
                (bind ((points (+ (length (members next-unit))
                                  (/ (* 100 (1+ ls)
                                        ls)
                                     2)))
                       (line-bonus (if (> ls-old 1)
                                       (floor (/ (* (1- ls-old)
                                                    points)
                                                 10))
                                       0)))
                  (incf move-score (+ points line-bonus)))

                (appending next-unit-frames into frames)

                (finally

;; calculate power score
;; power_scorep = 2 * lenp * repsp + power_bonusp
;;   where
;;   power_bonusp = if repsp > 0
;;                  then 300
;;                  else 0
                 (bind ((script-copy (copy-seq script)))
                   (iter
                     (for (phrase-script . phrase-text) in power-phrases-alist)
                     (for phrase-length = (length phrase-script))
                     (iter
                       (for pos
                            initially (search phrase-script script-copy)
                            then (search phrase-script script-copy :start2 (+ pos phrase-length)))
                       (unless pos
                         (terminate))
                       (replace script-copy (make-list phrase-length) :start1 pos :end1 (+ pos phrase-length))
                       (incf power-score (* 2 phrase-length))
                       (unless (gethash phrase-script power-phrases-used)
                         (incf power-score 300)
                         (setf (gethash phrase-script power-phrases-used) t)))))

                 (return (values script frames move-score power-score)))))
      (list :game world :seed seed :script game-script :film film :move-score move-score :power-score power-score :score (+ move-score power-score)))))

(defmethod game-loop ((world game) &optional &key record-film time-limit memory-limit number-cores (phrases nil phrases-p))
  (unless phrases-p
    (setf phrases *power-phrases*))
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
