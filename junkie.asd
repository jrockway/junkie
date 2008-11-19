;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(defpackage #:junkie-asd
  (:use :cl :asdf))

(in-package #:junkie-asd)

(defsystem junkie
  :name "junkie"
  :depends-on (:fiveam :iterate)
  :components ((:file "package")
               (:file "junkie" :depends-on ("package"))
               (:file "tests" :depends-on ("junkie"))))
