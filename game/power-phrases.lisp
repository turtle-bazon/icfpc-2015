
(in-package :hextris)

(defparameter *power-phrases*
  '("ei!"
    "R'lyeh"
    "Yuggoth"
    "Ph'nglui mglw'nafh Cthulhu R'lyeh wgah'nagl fhtagn!"))

(defparameter *char-mapping* '((:w . "p'!.03")
                               (:e . "bcefy2")
                               (:sw . "aghij4")
                               (:se . "lmno 5")
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
