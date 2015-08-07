(defun make-rng (&key (seed 0))
  #'(lambda () (ash (setf seed (mod (+ (* seed 1103515245) 12345) (expt 2 31))) -16)))

(let ((rng (make-rng :seed 17))) (list (funcall rng) (funcall rng) (funcall rng) (funcall rng)))

