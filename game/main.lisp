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

(defun main (args)
  (destructuring-bind (files time-limit memory-limit number-cores phrases)
      (parse-args (rest args))
    (declare (ignore memory-limit time-limit))
    (let* ((solutions (iter (for fname in files)
                            (for game = (parse-input-file fname))
                            (appending
                             (game-loop game
                                        :time-limit time-limit
                                        :memory-limit memory-limit
                                        :number-cores number-cores
                                        :phrases phrases))))
           (result-string (json:encode-json-to-string
                           (iter (for s in solutions)
                                 (for game = (getf s :game))
                                 (for seed = (getf s :seed))
                                 (for script = (getf s :script))
                                 (collecting `((:problem-id . ,(problem-id game))
                                               (:seed . ,seed)
                                               (:solution . ,(power-phrase-encode-adt script))))))))
      (format t "~a~&" result-string))))
