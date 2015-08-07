
(in-package :hextris)

(defun parse-input-file (filename)
  (parse-input-json
   (with-open-file (in filename)
     (json:decode-json in))))

(defun parse-input-json (json)
  (flet ((field (name &optional (base json)) (cdr (assoc name base))))
    (let ((parsed-map (make-instance 'hextris-map :width (field :width) :height (field :height))))
      (iter (for cell-json in (field :filled))
            (setf (map-cell parsed-map (make-cell-row-col (field :y cell-json) (field :x cell-json))) t))
      (let ((units (iter (for unit-json in (field :units))
                         (for pivot-json = (field :pivot unit-json))
                         (for pivot-cell = (make-cell-row-col (field :y pivot-json)
                                                              (field :x pivot-json)))
                         (for members = (iter (for member-json in (field :members unit-json))
                                              (for member-cell = (make-cell-row-col (field :y member-json)
                                                                                    (field :x member-json)))
                                              (collect (make-cell :cube-x (- (cell-cube-x member-cell) (cell-cube-x pivot-cell))
                                                                  :cube-y (- (cell-cube-y member-cell) (cell-cube-y pivot-cell))
                                                                  :cube-z (- (cell-cube-z member-cell) (cell-cube-z pivot-cell))))))
                         (collect (make-instance 'unit :members members)))))
        (make-instance 'game
                       :seeds (field :source-seeds)
                       :units units
                       :map parsed-map)))))
