(in-package :common-lisp-user)

(defpackage :function-cache-test
  (:documentation "A simple caching layer for functions")
  (:use :common-lisp :iterate :function-cache :lisp-unit2))
