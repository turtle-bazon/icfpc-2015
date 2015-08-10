
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
  (declare (optimize (debug 3)))
  (flet ((try-escape (move)
           (let ((moved-unit (move-unit move final-position field)))
             (unless moved-unit
               (return-from gen-freeze-move move))
             (unless (place-on-map (unit-on-map-unit moved-unit) (unit-on-map-coord moved-unit) field)
               (return-from gen-freeze-move move)))))
    (iter (for move in *a-star-moves*)
          (try-escape move))))

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
  (pos nil :type (or null unit-on-map))
  (script nil :type list)
  (score 0.0 :type single-float)
  (pws (make-array 0 :element-type 'fixnum) :type (simple-array fixnum *))
  (weight 0.0 :type single-float))

(defun a*-st> (trans-a trans-b)
  (> (a*-st-weight trans-a) (a*-st-weight trans-b)))

(defparameter *distict-power-words-count-factor* 0.2)
(defparameter *total-power-words-count-factor* 0.1)

(defun a*-st-create (&key pos script field solver pws pws-inc pws-avail)
  (declare (optimize (debug 3)))
  (let* ((st-pws (if pws
                     (let ((pws-copy (copy-seq pws)))
                       (when pws-inc
                         (incf (elt pws-copy pws-inc)))
                       pws-copy)
                     (make-array (length pws-avail) :element-type 'fixnum :initial-element 0)))
         (score (estimate solver field pos))
         (weight (* score
                    (+ 1.0 (* (iter (for v in-vector st-pws) (counting (not (zerop v))))
                              *distict-power-words-count-factor*))
                    (+ 1.0 (* (iter (for v in-vector st-pws) (summing v))
                              *total-power-words-count-factor*)))))
    (make-a*-st :pos pos
                :script script
                :score score
                :pws st-pws
                :weight weight)))

(defun a*-moves-w/power-words ()
  (coerce (sort (append (mapcar 'list *a-star-moves*)
                        (mapcar #'car (power-phrases-alist *power-phrases*)))
                #'>
                :key #'length)
          'vector))

(defmethod run-a-star ((field hextris-map) (solver solver) (start-pos unit-on-map) &key limits-callback)
  (declare (optimize (debug 3)))
  (let* ((queue (priority-queue:make-pqueue #'a*-st> :key-type 'a*-st))
         (visited (make-instance 'visited-cache))
         (available-subtracks (a*-moves-w/power-words)))
    (priority-queue:pqueue-push t
                                (a*-st-create :pos start-pos
                                              :field field
                                              :solver solver
                                              :pws-avail available-subtracks)
                                queue)
    (mark-visited visited start-pos)
    (iter (with our-best-state = nil)
          ;; check stop condition
          (when (or (priority-queue:pqueue-empty-p queue)
                    (and limits-callback (functionp limits-callback) (funcall limits-callback)))
            (return (when our-best-state
                      (values t (reverse (a*-st-script our-best-state)) (a*-st-pos our-best-state)))))
          (for (values _ cur-state) = (priority-queue:pqueue-pop queue))
          ;; track best
          (when (and (or (not our-best-state)
                         (a*-st> cur-state our-best-state))
                     (gen-freeze-move field (a*-st-pos cur-state)))
            (setf our-best-state cur-state))
          ;; transitions
          (iter (for subtrack in-vector available-subtracks)
                (for subtrack-index from 0)
                ;; check full path is passable
                (for path = (iter (with cur-pos = (a*-st-pos cur-state))
                                  (for move in subtrack)
                                  (for moved-pos = (move-unit move cur-pos field))
                                  (unless (and moved-pos
                                               (place-on-map (unit-on-map-unit moved-pos) (unit-on-map-coord moved-pos) field)
                                               (not (fast-check visited moved-pos)))
                                    (return nil))
                                  (setf cur-pos moved-pos)
                                  (collect (cons move moved-pos))))
                (unless path
                  (next-iteration))
                ;; make states
                (iter (with iter-state = cur-state)
                      (for ((move . moved-pos) . rest-moves) on path)
                      (for next-state = (a*-st-create :pos moved-pos
                                                      :script (cons move (a*-st-script iter-state))
                                                      :field field
                                                      :solver solver
                                                      :pws (a*-st-pws iter-state)
                                                      :pws-inc (when (and (not rest-moves)
                                                                          (> (length subtrack) 1))
                                                                 subtrack-index)))
                      (priority-queue:pqueue-push t next-state queue)
                      (mark-visited visited (a*-st-pos next-state))
                      (setf iter-state next-state))))))
          
(defun debug-locate-visualize (filename)
  (let ((*info-printer* (lambda (field &key current-position final-position &allow-other-keys)
                          (if final-position
                              (format t " ;; FINAL estimate = ~a, find-out-burned-rows-count = ~a~%"
                                      (estimate (make-instance 'hedonistic-solver) field final-position)
                                      (find-out-burned-rows-count (unit-lock (unit-on-map-unit final-position)
                                                                             (unit-on-map-coord final-position)
                                                                             field)))
                              (format t " ;; CURRENT estimate = ~a~%"
                                      (estimate (make-instance 'hedonistic-solver) field current-position))))))
    (game-loop (parse-input-file filename) :record-film t)))
