;;; -*- hextris main -*-

(defpackage #:hextris-asd
  (:use :cl :asdf))

(in-package #:hextris-asd)

(defsystem hextris
  :name "hextris"
  :version "0.1"
  :author "skobochka"
  :depends-on (:iterate :metatilities :cl-json :drakma :lift)
  :components ((:file "package")
               (:file "main" :depends-on ("package" "parser"))
               (:file "power-phrases" :depends-on ("package"))
               (:file "rng" :depends-on ("package"))
               (:file "remote" :depends-on ("power-phrases"))
               (:file "map" :depends-on ("package"))
               (:file "unit" :depends-on ("map"))
               (:file "game" :depends-on ("unit" "rng"))
               (:file "parser" :depends-on ("game"))
               (:file "a-star" :depends-on ("game"))
               (:file "coverage" :depends-on ("a-star"))))

