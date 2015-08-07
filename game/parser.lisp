
(in-package :hextris)

(defun parse-input-file (filename)
  (parse-input-json
   (with-open-file (in filename)
     (json:decode-json in))))

(defun parse-input-json (json)
  (flet ((field (name &optional (base json)) (cdr (assoc name base))))
    (let ((parsed-map (make-instance 'hextris-map :width (field :width) :height (field :height))))
      (iter (for map-cell in (field :filled))
            (setf (cell parsed-map (field :y map-cell) (field :x map-cell)) t))
      (make-instance 'game
                     :seeds (field :source-seeds)
                     :map parsed-map))))
