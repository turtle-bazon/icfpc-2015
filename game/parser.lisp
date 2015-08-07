
(in-package :hextris)

(defun parse-input-file (filename)
  (parse-input-json
   (with-open-file (in filename)
     (json:decode-json in))))

(defun parse-input-json (json)
  (flet ((field (name) (cdr (assoc name json))))
    (make-instance 'game
                   :seeds (field :source-seeds)
                   :map (make-instance 'hextris-map :width (field :width) :height (field :height)))))
  
