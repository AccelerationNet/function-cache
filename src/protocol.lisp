(in-package :function-cache)

(defgeneric clear-cache (cache &optional args)
  (:documentation "Clears a given cache"))

(defgeneric purge-cache (cache)
  (:documentation "A function that will remove expired entries from the cache,
  allowing them to be garbage collected"))

(defgeneric get-cached-value (cache cache-key)
  (:documentation "returns the result-values-list and at what time it was cached"))

(defgeneric (setf get-cached-value) (new cache cache-key)
  (:documentation "Set the cached value for the cache key"))

(defgeneric at-cache-capacity? (cache)
  (:documentation "is the cache full?")
  (:method (cache) nil))

(defgeneric reduce-cached-set (cache n)
  (:documentation "evict n items from the cache"))

(defgeneric cached-results-count (cache)
  (:documentation "A function to compute the number of results that have been
  cached. DOES NOT CHECK to see if the entries are expired")
  (:method ((res list)) (length res))
  (:method ((res hash-table)) (hash-table-count res)))

(defgeneric make-cache-backing (cache)
  (:documentation "make a new backing storage for the cache")
  (:method (cache) nil))

(defgeneric expired? (cache result-timeout)
  (:documentation "Determines if the cache entry is expired")
  (:method (cache result-timeout)
    (let ((timeout (timeout cache)))
      (cond
        ;; things never expire
        ((null timeout) nil)
        ;; no valid cache entry - must be expiredish
        ((null result-timeout) t)
        ;; we have timeouts and times to compare, are we past expiration
        (t (let ((expires-at (+ timeout result-timeout)))
             (<= expires-at (get-universal-time))))
        ))))

(defgeneric defcached-hashkey (thing)
  (:documentation "Turns a list of arguments into a valid cache-key
    (usually a tree of primatives)")
  (:method ((thing T))
    (typecase thing
      (null nil)
      (list (iter (for i in thing)
              (collect (defcached-hashkey i))))
      (t thing))))
