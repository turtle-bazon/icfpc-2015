(in-package :hextris)

(defvar *publish-api-uri* "https://davar.icfpcontest.org/teams/16/solutions")

(defvar *publish-api-key* "M72G+E7hDY2E9yoKXtsalHDF7lQTFwJY7oX/NunZzBg=")

(defun parse-args (args)
  (iter (initially (setq args-tail args))
        (for pname = (car args-tail))
        (for pvalue = (car (cdr args-tail)))
        (for args-tail next (if (eq args-tail nil) (terminate) (rest (rest args-tail))))
        (when (string= "-f" (string-downcase pname))
          (collect pvalue into files))
        (when (string= "-t" (string-downcase pname))
          (for time-limit = (parse-integer pvalue)))
        (when (string= "-m" (string-downcase pname))
          (for memory-limit = (parse-integer pvalue)))
        (when (string= "-c" (string-downcase pname))
          (for number-cores = (parse-integer pvalue)))
        (when (string= "-p" (string-downcase pname))
          (collect pvalue into phrases))
        (when (string= "-publish" (string-downcase pname))
          (when (or (string= "t" (string-downcase pvalue))
                    (string= "true" (string-downcase pvalue))
                    (string= "yes" (string-downcase pvalue)))
            (for publish = t)))
        (when (string= "-score-only" (string-downcase pname))
          (when (or (string= "t" (string-downcase pvalue))
                    (string= "true" (string-downcase pvalue))
                    (string= "yes" (string-downcase pvalue)))
            (for score-only = t)))
        (when (string= "-tag" (string-downcase pname))
          (for tag = (string-downcase pvalue)))
        (finally (return (list files time-limit memory-limit number-cores phrases publish score-only tag)))))

(defun execution-plan (files)
  (iter (for fname in files)
        (for game = (parse-input-file fname))
        (appending
         (iter (for seed in (seeds game))
               (collecting (list game seed))))))

(defun main (args)
  (in-package :hextris)
  (sb-sys:enable-interrupt
   sb-unix:sigint
   (lambda (&rest args)
     (declare (ignore args))
     (sb-sys:invoke-interruption
      (lambda ()
        (sb-sys:with-interrupts
          (sb-ext:exit)) ()))))
  (destructuring-bind (files time-limit memory-limit number-cores phrases
                             publish score-only tag)
      (parse-args (rest args))
    (let ((*power-phrases* phrases)
          (solutions '())
          (solutions-lock (bordeaux-threads:make-lock "solutions"))
          (tpool (thread-pool:make-fixed-thread-pool "main-game"
                                                     :size (or number-cores 1)))
          (executions (execution-plan files)))
      (thread-pool:start-pool tpool)
      (iter (for (game seed) in executions)
            (let ((current-game game)
                  (current-seed seed))
              (thread-pool:execute tpool
                                   (lambda ()
                                     (let ((result
                                            (single-game-loop current-game current-seed
                                                              :time-limit time-limit
                                                              :memory-limit memory-limit
                                                              :number-cores number-cores
                                                              :phrases phrases)))
                                       (bordeaux-threads:with-lock-held (solutions-lock)
                                         (push result solutions)))))))
      (thread-pool:stop-pool tpool)
      (if score-only
          (iter (for s in (sort solutions #'>
                                :key (lambda (v)
                                       (+ (* 100 (problem-id (getf v :game)))
                                          (getf v :seed)))))
                (for game = (getf s :game))
                (for seed = (getf s :seed))
                (for score = (getf s :score))
                (format t "G: ~a, S: ~a, Sc: ~a~&"
                        (problem-id game) seed score))
          (let ((result-string (json:encode-json-to-string
                                (iter (for s in solutions)
                                      (for game = (getf s :game))
                                      (for seed = (getf s :seed))
                                      (for script = (getf s :script))
                                      (collecting `((:problem-id . ,(problem-id game))
                                                    (:seed . ,seed)
                                                    (:solution . ,(power-phrase-encode-adt script))
                                                    ,@(when tag `((:tag . ,(format nil "~a-~a-~a"
                                                                                   tag
                                                                                   (problem-id game)
                                                                                   (get-internal-real-time)))))))))))
            (if publish
                (format t "~a~&"
                        (http-request *publish-api-uri*
                                      :method :post
                                      :basic-authorization `("" ,*publish-api-key*)
                                      :content-type "application/json"
                                      :content result-string)))
            (format t "~a~&" result-string))))))
