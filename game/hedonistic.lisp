
(in-package :hextris)

(defclass hedonistic-solver (solver)
  ())

(defparameter *sum-of-heights-factor* -0.03)
(defparameter *row-burn-factor* 8.0)
(defparameter *blockade-factor* -3.5)
(defparameter *touching-something-factor* 3.0)
(defparameter *touching-wall-factor* 2.5)
(defparameter *touching-floor-factor* 5.0)

(defun find-out-sum-of-heights (field)
  ;; TODO
  )

(defun find-out-burned-rows-count (field)
  ;; TODO
  )

(defun find-out-blockades-count (field)
  ;; TODO
  )

(defun members-touching-something-count (pos field)
  ;; TODO
  )

(defun members-touching-wall-count (pos field)
  ;; TODO
  )

(defun members-touching-floor-count (pos field)
  ;; TODO
  )

(defmethod estimate ((solver hedonistic-solver) (field hextris-map) (checking-pos unit-on-map))
  (+ (* (find-out-sum-of-heights field) *sum-of-heights-factor*)
     (* (find-out-burned-rows-count field) *row-burn-factor*)
     (* (find-out-blockades-count field) *blockade-factor*)
     (* (members-touching-something-count checking-pos field) *touching-something-factor*)
     (* (members-touching-wall-count checking-pos field) *touching-wall-factor*)
     (* (members-touching-floor-count checking-pos field) *touching-floor-factor*)))

  
