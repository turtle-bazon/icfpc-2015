(in-package :hextris)

(defun parse-args (args)
  (iter (initially (setq args-tail args))
        (for pname = (car args-tail))
        (for pvalue = (car (cdr args-tail)))
        (for args-tail next (if (eq args-tail nil) (terminate) (rest (rest args-tail))))
        (when (string= "-f" (string-downcase pname))
          (collect pvalue into files))
        (when (String= "-t" (string-downcase pname))
          (for time-limit = (parse-integer pvalue)))
        (when (String= "-m" (string-downcase pname))
          (for memory-limit = (parse-integer pvalue)))
        (when (String= "-c" (string-downcase pname))
          (for number-cores = (parse-integer pvalue)))
        (when (string= "-p" (string-downcase pname))
          (collect pvalue into phrases))
        (finally (return (list files time-limit memory-limit number-cores phrases)))))

(defun run-game (game seed number-cores phrases)
  1)

(defun main (args)
  (destructuring-bind (files time-limit memory-limit number-cores phrases)
      (parse-args (rest args))
    (let ((solutions (iter (for fname in files)
                           (for game = (parse-input-file fname))
                           (appending
                               (iter (for seed in (seeds game))
                                     (collect (run-game game seed number-cores phrases)))))))
      (format t "~a~&" solutions))))
