(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :function-cache-system)
    (defpackage :function-cache-system
      (:use :common-lisp :asdf))))

(in-package :function-cache-system)

(defsystem :function-cache-clsql
    :description "A Simple Caching Layer for functions"
    :author "Acceleration.net <programmers@acceleration.net>"
    :licence "BSD"
    :version "1.0"
    :components
    ((:module :src
              :serial T
              :components
              ((:file "clsql"))))
    :depends-on (:function-cache :clsql :clsql-helper))