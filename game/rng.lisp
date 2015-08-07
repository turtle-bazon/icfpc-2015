(in-package :hextris)

(defun make-rng (seed)
  #'(lambda ()
      (let ((random-base seed))
        (setf seed (mod (+ (* seed 1103515245) 12345) (expt 2 31)))
        (ash random-base -16))))

