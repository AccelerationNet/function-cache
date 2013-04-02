(in-package :common-lisp-user)

(defpackage :function-cache
  (:documentation "A simple caching layer for functions")
  (:use :common-lisp :iterate)
  (:shadowing-import-from :alexandria :ensure-list )
  (:export
   #:defcached
   #:compute-cache-key
   #:defcached-hashkey
   #:get-cached-value
   #:dont-care
   #:clear-cache
   #:clear-all-caches
   #:clear-cache-partial-arguments
   #:cached-results-count
   #:partial-argument-match?
   #:purge-cache
   #:purge-all-caches
   #:timeout
   #:cached-results
   #:clear-cache-all-function-caches
   #:*cache-names*
   #:hash-table-function-cache
   #:single-cell-function-cache
   #:thunk-cache
   ))