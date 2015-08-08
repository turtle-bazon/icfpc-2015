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
        (finally (return (list files time-limit memory-limit number-cores phrases publish)))))

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
  (destructuring-bind (files time-limit memory-limit number-cores phrases publish)
      (parse-args (rest args))
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
      (if publish
          (format t "~a~&"
                  (http-request *publish-api-uri*
                                :method :post
                                :basic-authorization `("" ,*publish-api-key*)
                                :content-type "application/json"
                                :content result-string))
          (format t "~a~&" result-string)))))
