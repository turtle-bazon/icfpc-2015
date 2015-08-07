;;; -*- hextris main -*-

(defpackage #:hextris-asd
  (:use :cl :asdf))

(in-package #:hextris-asd)

(defsystem hextris
  :name "hextris"
  :version "0.1"
  :author "skobochka"
  :depends-on (:iterate :metatilities :cl-json)
  :components ((:file "package")
               (:file "map" :depends-on ("package"))
               (:file "parser" :depends-on ("map"))))

