
(in-package :hextris)

(defun parse-input-file (filename)
  (parse-input-json
   (with-open-file (in filename)
     (json:decode-json in))))

(defun parse-input-json (json)
  (flet ((field (name &optional (base json)) (cdr (assoc name base))))
    (let ((parsed-map (make-instance 'hextris-map :width (field :width) :height (field :height))))
      (iter (for cell-json in (field :filled))
            (setf (map-cell parsed-map (make-cell :row (field :y cell-json) :col (field :x cell-json))) t))
      (let ((units (iter (for unit-json in (field :units))
                         (for pivot-json = (field :pivot unit-json))
                         (for pivot-row = (field :y pivot-json))
                         (for pivot-col = (field :x pivot-json))
                         (for members = (iter (for member-json in (field :members unit-json))
                                              (collect (make-cell :row (- (field :y member-json) pivot-row)
                                                                  :col (- (field :x member-json) pivot-col)))))
                         (collect (make-instance 'unit :members members)))))
        (make-instance 'game
                       :seeds (field :source-seeds)
                       :units units
                       :map parsed-map)))))
