
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

(defmethod gen-freeze-move ((field hextris-map) (final-position unit-on-map) &key script phrases)
  (declare (optimize (debug 3)) (ignore script phrases))
  (flet ((try-escape (move)
           (let ((moved-unit (move-unit move final-position field)))
             (unless moved-unit
               (return-from gen-freeze-move move))
             (unless (place-on-map (unit-on-map-unit moved-unit) (unit-on-map-coord moved-unit) field)
               (return-from gen-freeze-move move)))))
    ;; (when script ;;; let's try to finish with powerword!
    ;;   (let ((pws-rev (sort (mapcar (compose #'reverse #'car) (power-phrases-alist phrases)) #'> :key #'length))
    ;;         (script-rev (reverse script)))
    ;;     (iter (while pws-rev)
    ;;           (for next-pw-rev = '())
    ;;           (iter (for (pw-move . pw-rev-tail) in pws-rev)
    ;;                 (when (and (>= (length script-rev) (length pw-rev-tail))
    ;;                            (equal pw-rev-tail (subseq script-rev 0 (length pw-rev-tail))))
    ;;                   (try-escape pw-move))
    ;;                 (when pw-rev-tail
    ;;                   (push (cdr pw-rev-tail) next-pw-rev)))
    ;;           (setf pws-rev (reverse next-pw-rev)))))
    (iter (for move in *a-star-moves*) ;;; locate freeze move in standard way
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
  pos
  script
  mmarks)

(defmethod transition-better-p (end-pos)
  (flet ((distinct-count (pos)
           (iter (for bit in-vector (a*-st-mmarks pos)) (counting (not (zerop bit))))))
    (lambda (trans-a trans-b)
      (let ((uniq-count-a (distinct-count trans-a))
            (uniq-count-b (distinct-count trans-b)))
        (or (> uniq-count-a uniq-count-b)
            (and (= uniq-count-a uniq-count-b)
                 (or (> (length (car (a*-st-script trans-a)))
                        (length (car (a*-st-script trans-b))))
                     (and (= (length (car (a*-st-script trans-a)))
                             (length (car (a*-st-script trans-b))))
                          (funcall (position-better-p end-pos)
                                   (a*-st-pos trans-a)
                                   (a*-st-pos trans-b))))))))))

(defun a*-moves-w/power-words (phrases)
  (coerce (append (mapcar 'list *a-star-moves*)
                  (mapcar #'car (power-phrases-alist phrases)))
          'vector))

(defun make-mmarks (available-subtracks)
  (make-array (length available-subtracks) :element-type 'bit))

(defmethod run-a-star ((field hextris-map) (start-pos unit-on-map) (end-pos unit-on-map) phrases)
  (declare (optimize (debug 3)))
  (let ((queue (priority-queue:make-pqueue (transition-better-p end-pos) :key-type 'a*-st))
        (visited (make-instance 'visited-cache))
        (available-subtracks (a*-moves-w/power-words phrases)))
    (priority-queue:pqueue-push t (make-a*-st :pos start-pos :script '() :mmarks (make-mmarks available-subtracks)) queue)
    (mark-visited visited start-pos)
    (iter (until (priority-queue:pqueue-empty-p queue))
          (for (values _ cur-state) = (priority-queue:pqueue-pop queue))
          (when (positions= (a*-st-pos cur-state) end-pos)
            (return-from run-a-star (values t (flatten (reverse (a*-st-script cur-state))))))
          (for transitions+paths =
               (iter (for subtrack in-vector available-subtracks)
                     (for subtrack-index from 0)
                     (for path = 
                          (iter (with current-pos = (a*-st-pos cur-state))
                                (for move in subtrack)
                                (for moved-pos = (move-unit move current-pos field))
                                (unless (and moved-pos (place-on-map (unit-on-map-unit moved-pos) (unit-on-map-coord moved-pos) field))
                                  (return nil))
                                (when (or (positions= current-pos moved-pos) (find moved-pos path :test #'positions= :key #'cdr))
                                  (return nil))
                                (collect (cons move moved-pos) into path)
                                (setf current-pos moved-pos)
                                (finally (return path))))
                     (when path
                       (collect (cons (make-a*-st :pos (cdar (last path))
                                                  :script (cons (mapcar #'car path) (a*-st-script cur-state))
                                                  :mmarks (let ((v (copy-seq (a*-st-mmarks cur-state))))
                                                            (setf (elt v subtrack-index) 1)
                                                            v))
                                      (mapcar #'cdr path))))))
          (for sorted-transitions+paths = (sort transitions+paths (transition-better-p end-pos) :key #'car))
          (iter (for (trans . history) in sorted-transitions+paths)
                (for visited-p = (some (curry #'fast-check visited) history))
                (unless visited-p
                  (priority-queue:pqueue-push t trans queue)
                  (iter (for pos in history) (mark-visited visited pos)))))))
          
          
