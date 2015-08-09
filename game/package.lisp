(defpackage #:hextris
  (:use :cl :iterate :metatilities :drakma)
  (:shadowing-import-from :metatilities minimize finish)
  (:export
    #:main))

(in-package :hextris)

(defvar *info-printer*)

