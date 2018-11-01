(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :function-cache-system)
    (defpackage :function-cache-system
      (:use :common-lisp :asdf))))

(in-package :function-cache-system)

(defsystem :function-cache
    :description "A Simple Caching Layer for functions"
    :author "Acceleration.net <programmers@acceleration.net>"
    :licence "BSD"
    :version "1.0.3"
    :components
    ((:module :src
              :serial T
              :components
              ((:file "packages")
               (:file "protocol")
               ;; mixins
               (:file "capacity")
               (:file "metering")
               ;; cachers
               (:file "function-cache")
               (:file "thunk")
               (:file "single-cell")
               (:file "hash-table")
               (:file "ordered")
               ;; driver
               (:file "cache")
               )))
    :depends-on (:alexandria :cl-interpol :iterate :symbol-munger :closer-mop))

(asdf:defsystem function-cache/test
  :description "tests for function cache"
  :version "1.0.3"
  :depends-on (:function-cache :lisp-unit2)
  :components ((:module :test
                        :serial T
                        :components
                        ((:file "packages")
                         (:file "function-cache")
                         (:file "metering")))))

(defmethod asdf:perform ((o asdf:test-op) (c (eql (find-system :function-cache))))
  (asdf:oos 'asdf:load-op :function-cache/test)
  (let ((*package* (find-package :function-cache-test)))
    (eval (read-from-string "
             (run-tests :package :function-cache-test
                        :name :function-cache
                        :run-contexts #'with-summary-context)
      "))))

;; Copyright (c) 2013 Russ Tyndall , Acceleration.net
;; http://www.acceleration.net All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are
;; met:
;;
;;  - Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;;
;;  - Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;; A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT
;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
