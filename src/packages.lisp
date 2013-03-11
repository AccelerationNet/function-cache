(in-package :common-lisp-user)

(defpackage :function-cache
  (:documentation "A simple caching layer for functions")
  (:use :common-lisp :iterate)
  (:shadowing-import-from :alexandria :ensure-list )
  (:export
   #:defcached
   #:defcached*
   #:defcached-hashkey
   #:defcached-thunk
   #:defcached-shared
   #:clear-cache
   #:timeout
   #:cached-results
   #:clear-cache-all-function-caches
   #:*cache-names*
   #:hash-table-function-cache
   #:shared-hash-table-function-cache
   ))