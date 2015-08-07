;;; -*- hextris main -*-

(defpackage #:hextris-asd
  (:use :cl :asdf))

(in-package #:hextris-asd)

(defsystem hextris
  :name "hextris"
  :version "0.1"
  :author "skobochka"
  :depends-on (:iterate :metatilities :cl-json :drakma)
  :components ((:file "package")
               (:file "map" :depends-on ("package"))
               (:file "remote" :depends-on ("package"))
               (:file "unit" :depends-on ("map"))
               (:file "game" :depends-on ("unit"))
               (:file "parser" :depends-on ("game"))))

