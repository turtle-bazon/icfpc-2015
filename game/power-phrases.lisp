
(in-package :hextris)

(defparameter *power-phrases*
  '("Ei!"
    "Ia! Ia!"
    "Yuggoth"
    "R'lyeh"
    "Yogsothoth"
    "Tsathoggua"
    "John BigBoote"
    "Planet 10"))

(defparameter *char-mapping* '((:w .   "p'!.03")
                               (:e .   "bcefy2")
                               (:sw .  "aghij4")
                               (:se .  "lmno 5")
                               (:rcw . "dqrvz1")
                               (:rcc . "kstuwx")))

(defun power-phrases-build-hash (cm)
  (bind ((hash (make-hash-table)))
    (iter (for (op . str) in cm)
          (iter
            (for char in-string str)
            (setf (gethash char hash) op)))

    hash))

(defparameter *char-mapping-hash* (power-phrases-build-hash *char-mapping*))

(defun power-phrases-alist (phrase-list)
  (iter (for phrase in phrase-list)
        (collect (cons (iter (for char in-string (string-downcase phrase))
                             (collect (gethash char *char-mapping-hash*)))
                       phrase))))


(defun power-phrase-encode-adt (adt)
  "Translate adt to string representation using as many power phases as itit possible"
  (bind ((phrase-map (power-phrases-alist *power-phrases*))
         ;; (phrase-map (sort phrase-map #'> :key #'(lambda (x)
         ;;                                           (length (cdr x)))))
         (adt (copy-seq adt)))
    (iter
      (for continue = nil)
      (iter (for (phrase-adt . phrase-text) in phrase-map)
            (for pos = (search phrase-adt adt))
            (when pos
              (setf (subseq adt pos (+ pos (length phrase-adt))) phrase-text
                    continue t)))
      (while continue)
      (setf continue nil))

    (format nil "~{~a~}" (mapcar #'(lambda (x)
                                     (if (keywordp x)
                                         (elt (cdr (assoc x *char-mapping*)) 0)
                                         x))
                                 adt))))

(defparameter *power-phrase-check-solution-counter* 0)

(defun power-phrase-check-solution (input phrase &optional submit)
  (bind ((*power-phrases* (list phrase))
         (solution (power-phrase-encode-adt (getf (car (game-loop (parse-input-file input)))
                                                  :script))))
    (if submit
        (remote-submit-raw 0 0 solution (format nil "alterator-~a" (incf *power-phrase-check-solution-counter*)))
        solution)))
