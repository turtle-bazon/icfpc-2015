(in-package :hextris)

(defun make-rng (seed)
  (let ((value 0))
    #'(lambda () 
        (let ((random-value value))
          (setf value (ash (setf seed (mod (+ (* seed 1103515245) 12345) (expt 2 31))) -16))
          random-value))))

