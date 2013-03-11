(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :function-cache-system)
    (defpackage :function-cache-system
      (:use :common-lisp :asdf))))

(in-package :function-cache-system)

(defsystem :function-cache
    :description "A Simple Caching Layer for functions"
    :author "Acceleration.net <programmers@acceleration.net>"
    :licence "BSD"
    :version "1.0"
    :components
    ((:module :src
              :serial T
              :components
              ((:file "packages")
               (:file "function-cache"))))
    :depends-on (:alexandria :cl-interpol :iterate
                 :split-sequence :symbol-munger))

(asdf:defsystem function-cache-test
  :description "the part of adwcode"
  :depends-on (:function-cache :lisp-unit)
  :components ((:module :test
                        :serial T
                        :components
                        ((:file "packages")
                         (:file "function-cache")))))