(in-package :common-lisp-user)

(defpackage :function-cache
  (:documentation "A simple caching layer for functions")
  (:use :common-lisp :iterate)
  (:shadowing-import-from :alexandria :ensure-list )
  (:export
   #:*default-cache-class*
   #:*bypass-cache*
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
   #:cache-capacity-mixin
   #:at-cache-capacity?
   #:capacity
   #:reduce-by-ratio
   #:reduce-cached-set
   #:hash-table-function-cache
   #:hash-table-function-cache-with-capacity
   #:lru-cache
   #:mru-cache
   #:single-cell-function-cache
   #:thunk-cache
   #:find-function-cache-for-name
   ;; metering symbols
   #:metered-mixin
   #:hits
   #:misses
   #:hit-ratio
   #:reset-counters
   #:accesses
   ))
