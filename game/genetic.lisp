(in-package :hextris)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *chromosome-keys* '(*sum-of-heights-factor*
                                    *row-burn-factor*
                                    *blockade-factor*
                                    *touching-something-factor*
                                    *touching-wall-factor*
                                    *touching-floor-factor*
                                    *row-fill-factor*
                                    *distict-power-words-count-factor*
                                    *total-power-words-count-factor*
                                    *one-burnt-row-penalty*
                                    )))

(defmacro with-chromosome ((chromosome) &body body)
  (bind ((var (gensym)))
    `(bind ((,var ,chromosome)
            ,@(mapcar #'(lambda (x)
                          `(,x (getf ,var ',x)))
                      *chromosome-keys*))
       ,@body)))

(defun genetic-fitness (worlds phrases chromosome)
  (with-chromosome (chromosome)
    (iter (for world in (if (listp worlds) worlds (list worlds)))
          (sum (iter (for seed-result in (game-loop world :phrases phrases))
                     (sum (getf seed-result :move-score)))))))

(defun genetic-make-random-property (&optional (stretch 10.0))
  (- stretch (random (* 2 stretch))))

(defun genetic-make-random ()
  (mapcan #'(lambda (x)
              (list x (genetic-make-random-property)))
          *chromosome-keys*))

(defun genetic-make-random-population (size)
  (iter (repeat size)
        (collect (list 0 (genetic-make-random)))))

(defun genetic-breed-pair-random (key c1 c2)
  (getf (if (< (random 1.0) 0.5)
            c1
            c2)
        key))

(defun genetic-breed-pair-average (key c1 c2)
  (/ (+ (getf c1 key) (getf c2 key)) 2))

(defun genetic-breed (population needed-size breed-op)
  (iter (with len = (length population))
        (repeat needed-size)
        (for c1 = (second (nth (random len) population)))
        (for c2 = (second (nth (random len) population)))
        (collect (list 0
                       (iter (for key in *chromosome-keys*)
                             (appending (list key
                                              (if (< (random 1.0) 0.1)
                                                  (genetic-make-random-property)
                                                  (funcall breed-op key c1 c2)))))))))

(defun genetic-next-population (population extinction-factor breed-op)
  (bind ((survived (subseq population
                           0 (truncate (* (length population)
                                          extinction-factor)))))
    (genetic-breed survived (length population) breed-op)))


(defun genetic-fit-population (world population number-of-cores phrases)
  (bind ((tpool (thread-pool:make-fixed-thread-pool "genetic-ai-improver"
                                                    :size number-of-cores))
         (new-population (list))
         (population-lock (bordeaux-threads:make-lock "population")))

    (thread-pool:start-pool tpool)

    (iter (for (_ chromosome) in population)
          (bind ((the-world world)
                 (the-chromosome chromosome)
                 (the-phrases phrases))
            (thread-pool:execute tpool
                               (lambda ()
                                 (bind ((result (list (genetic-fitness the-world the-phrases the-chromosome) the-chromosome)))
                                   (bordeaux-threads:with-lock-held (population-lock)
                                     (push result new-population)))))))
    (thread-pool:stop-pool tpool)

    (sort new-population #'> :key #'first)))


(defun genetic-run (world number-of-generations
                    &key (population nil) (population-size 10) (extinction-factor 0.5)
                      (breed-op #'genetic-breed-pair-random) (number-of-cores 1)
                      phrases)

  (unless population
    (setf population (genetic-make-random-population population-size)))

  (iter (for gen from 0 to number-of-generations)
        (format t "Generation ~a...~%" gen)
        (setf population (genetic-fit-population world population number-of-cores phrases))
        (format t "Population: ~a~%" population)
        (setf population (genetic-next-population population extinction-factor breed-op))
        (finally (return population))))
