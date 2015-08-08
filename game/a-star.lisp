
(in-package :hextris)

(defparameter *a-star-moves* '(:w :sw :se :e :rcw :rcc))

(defstruct unit-on-map
  (unit nil :type (or null unit))
  (coord nil :type (or null cell)))

(defun cell->list (cell)
  (list (cell-cube-x cell) (cell-cube-y cell) (cell-cube-z cell)))

(defun position->points-list (pos)
  (cons (cell->list (unit-on-map-coord pos))
        (mapcar #'cell->list (position->vec pos))))

(defun position->vec (pos)
  (sort (copy-seq (members (unit-on-map-unit pos))) #'cell<))

;;; TODO: find out optimization (without sort)
(defun positions= (pos-a pos-b)
  (and (cell= (unit-on-map-coord pos-a) (unit-on-map-coord pos-b))
       (every #'cell= (position->vec pos-a) (position->vec pos-b))))

(defmethod move-unit (move (obj unit-on-map) (field hextris-map))
  (ecase move
    ((:w :sw :se :e)
     (make-unit-on-map :unit (unit-on-map-unit obj) :coord (cell-move (unit-on-map-coord obj) move)))
    ((:rcw :rcc)
     (make-unit-on-map :unit (unit-rotate (unit-on-map-unit obj) move) :coord (unit-on-map-coord obj)))))


(defmethod gen-freeze-move ((field hextris-map) (final-position unit-on-map))
  (iter (for move in *a-star-moves*)
        (for moved-unit = (move-unit move final-position field))
        (unless moved-unit
          (return-from gen-freeze-move move))
        (unless (place-on-map (unit-on-map-unit moved-unit) (unit-on-map-coord moved-unit) field)
          (return-from gen-freeze-move move))))

(defun sq-dist (cell-a cell-b)
  (declare (optimize (speed 3))
           (type cell cell-a cell-b))
  (+ (* (- (cell-cube-x cell-b) (cell-cube-x cell-a))
        (- (cell-cube-x cell-b) (cell-cube-x cell-a)))
     (* (- (cell-cube-y cell-b) (cell-cube-y cell-a))
        (- (cell-cube-y cell-b) (cell-cube-y cell-a)))
     (* (- (cell-cube-z cell-b) (cell-cube-z cell-a))
        (- (cell-cube-z cell-b) (cell-cube-z cell-a)))))

(defmethod position-better-p (end-pos)
  (lambda (pos-a pos-b)
    (< (sq-dist (unit-on-map-coord pos-a) (unit-on-map-coord end-pos))
       (sq-dist (unit-on-map-coord pos-b) (unit-on-map-coord end-pos)))))

(defclass visited-cache ()
  ((level-1 :initform (make-hash-table :test 'eql) :reader level-1)))

(defun cell-sxhash (cell)
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type cell cell))
  (sxhash (list (cell-cube-x cell) (cell-cube-y cell) (cell-cube-z cell))))

(defun position-sxhash (pos)
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type unit-on-map pos))
  (let ((seed (cell-sxhash (unit-on-map-coord pos))))
    (declare (type (unsigned-byte 62) seed))
    (iter (declare (declare-variables))
          (for cell in (members (unit-on-map-unit pos)))
          (for (the (unsigned-byte 62) hash) = (cell-sxhash cell))
          (setf seed (logxor seed hash)))
    seed))

(defmethod mark-visited ((cache visited-cache) (pos unit-on-map) &key level-1-key)
  (let ((level-1-key (or level-1-key (position-sxhash pos))))
    (setf (gethash level-1-key (level-1 cache)) t)))

(defmethod fast-check ((cache visited-cache) (pos unit-on-map))
  (let ((level-1-key (position-sxhash pos)))
    (values (gethash level-1-key (level-1 cache)) level-1-key)))

(defstruct a*-st
  pos
  script
  pws)

(defun pw-track> (pw-track-a pw-track-b)
  (or (and pw-track-a (not pw-track-b))
      (and (and pw-track-a pw-track-b)
           (or (> (car pw-track-a) (car pw-track-b))
               (and (= (car pw-track-a) (car pw-track-b))
                    (> (length (cdr pw-track-a)) (length (cdr pw-track-b))))))))

(defun pw-track= (pw-track-a pw-track-b)
  (or (and (not pw-track-a) (not pw-track-b))
      (and (and pw-track-a pw-track-b)
           (= (car pw-track-a) (car pw-track-b))
           (= (length (cdr pw-track-a)) (length (cdr pw-track-b))))))

(defun best-pw-track (pw-tracks)
  (iter (with best = nil)
        (for pw-track in pw-tracks)
        (when (pw-track> pw-track best)
          (setf best pw-track))
        (finally (return best))))

(defmethod transition-better-p (end-pos)
  (lambda (trans-a trans-b)
    (let ((pw-track-a (best-pw-track (a*-st-pws trans-a)))
          (pw-track-b (best-pw-track (a*-st-pws trans-b))))
      (or (pw-track> pw-track-a pw-track-b)
          (and (pw-track= pw-track-a pw-track-b)
               (funcall (position-better-p end-pos)
                        (a*-st-pos trans-a)
                        (a*-st-pos trans-b)))))))

(defmethod run-a-star ((field hextris-map) (start-pos unit-on-map) (end-pos unit-on-map))
  (declare (optimize (debug 3)))
  (let ((queue (priority-queue:make-pqueue (transition-better-p end-pos) :key-type 'a*-st))
        (visited (make-instance 'visited-cache))
        (pw-starts (mapcar #'car (power-phrases-alist *power-phrases*))))
    (priority-queue:pqueue-push t (make-a*-st :pos start-pos :script '() :pws '()) queue)
    (mark-visited visited start-pos)
    (iter (until (priority-queue:pqueue-empty-p queue))
          (for (values _ cur-state) = (priority-queue:pqueue-pop queue))
          (when (positions= (a*-st-pos cur-state) end-pos)
            (return-from run-a-star (values t (reverse (a*-st-script cur-state)))))
          (for transitions = (iter (for move in *a-star-moves*)
                                   (for moved-unit = (move-unit move (a*-st-pos cur-state) field))
                                   (when moved-unit
                                     (let ((pw-tracks '()))
                                       ;; start new power phrases tracks
                                       (iter (for (pw-move . rest-pw-moves) in pw-starts)
                                             (when (eq move pw-move)
                                               (push (cons 1 rest-pw-moves) pw-tracks)))
                                       ;; continue existing power phrases tracks
                                       (iter (for (pw-count . pw-track) in (a*-st-pws cur-state))
                                             (when pw-track
                                               (for (pw-move . rest-pw-moves) = pw-track)
                                               (when (eq move pw-move)
                                                 (push (cons (1+ pw-count) rest-pw-moves) pw-tracks))))
                                       (for trans = (make-a*-st :pos moved-unit
                                                                :script (cons move (a*-st-script cur-state))
                                                                :pws pw-tracks)))
                                     (collect trans))))
          (iter (for trans in transitions)
                (multiple-value-bind (visited-p level-1-key)
                    (fast-check visited (a*-st-pos trans))
                  (unless visited-p
                    (when (place-on-map (unit-on-map-unit (a*-st-pos trans)) (unit-on-map-coord (a*-st-pos trans)) field)
                      (priority-queue:pqueue-push t trans queue)
                      (mark-visited visited (a*-st-pos trans) :level-1-key level-1-key))))))))
          
          
