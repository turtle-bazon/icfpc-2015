
(in-package :hextris)

(defparameter *bfs-max-depth* 384)

(defmethod single-game-loop ((world game) seed &optional
                             &key record-film time-limit memory-limit number-cores phrases)

  (declare (optimize (debug 3))
           (ignore time-limit memory-limit number-cores))
  (let ((rng (make-rng seed))
        (power-phrases-alist (power-phrases-alist phrases)))
    (multiple-value-bind (game-script move-score power-score)
        (iter (with current-map = (game-map world))
              (with move-score = 0)
              (with power-score = 0)
              (with power-phrases-used = (make-hash-table :test #'equal))
              (with solver = (make-instance 'hedonistic-solver))
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
              (multiple-value-bind (solution-exists-p moves-script final-position)
                  (let ((current-depth 0))
                    (run-a-star current-map solver init-position-on-map
                                :limits-callback (lambda () (>= (incf current-depth) *bfs-max-depth*))
                                :phrases phrases))
                (unless solution-exists-p
                  (terminate))
                ;;; record film (visualize)
                (when record-film
                  (iter (with actor = init-position-on-map)
                        (for move in moves-script)
                        (format t "~a~%" move)
                        (debug-draw current-map :current-position actor :final-position final-position)
                        (format t "---------------------~%")
                        (format t "move score: ~a power score: ~a~%SCORE: ~a~%"
                                move-score power-score (+ move-score power-score))
                        (break)
                        (setf actor (move-unit move actor current-map))))
;;; generate freeze move
                (for freeze-move = (gen-freeze-move current-map final-position))
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
                 (return (values script move-score power-score)))))
      (list :game world :seed seed :script game-script :move-score move-score :power-score power-score :score (+ move-score power-score)))))

(defmethod game-loop ((world game) &optional
                      &key record-film time-limit memory-limit number-cores phrases)
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
