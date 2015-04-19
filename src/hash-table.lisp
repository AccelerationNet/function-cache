(in-package :function-cache)

(defvar *default-hash-init-args*
  `(:test equal
    #+sbcl ,@'(:synchronized t)
    ))

(defclass hash-table-function-cache (function-cache)
  ((hash-init-args
    :accessor hash-init-args
    :initform *default-hash-init-args*
    :initarg :hash-init-args))
  (:documentation "a function cache that uses a hash-table to store results"))

(defmethod make-cache-backing ((cache hash-table-function-cache))
  (apply #'make-hash-table (hash-init-args cache)))

(defmethod cached-results-count ((cache hash-table-function-cache)
                                 &aux (cached (cached-results cache)))
  (when cached
    (hash-table-count cached)))

(defmethod get-cached-value ((cache hash-table-function-cache) cache-key)
  ;; if we get no hash when we expect one then it probably means that we
  ;; should just run the body (eg: http-context cached results require a valid
  ;; http context)
  (let* ((hash (cached-results cache))
         (cons (when hash (gethash cache-key (cached-results cache))))
         (res (car cons))
         (cached-at (cdr cons)))
    (values res cached-at)))

(defmethod (setf get-cached-value) (new (cache hash-table-function-cache) cache-key)
  ;; without our shared hash, we cannot cache
  (let ((hash (cached-results cache)))
    (when hash (setf (gethash cache-key hash)
                     (cons new *cached-at*))))
  new)

(defmethod key-cached? ((cache hash-table-function-cache) cache-key)
  (let* ((hash (cached-results cache)))
    (nth-value 1 (gethash cache-key hash))))

(defgeneric partial-argument-match? (cache cached-key to-match
                                     &key test)
  (:documentation "Trys to see if the cache-key matches the to-match partial
   key passed in.

   The basic implementation is to go through the cache-keys and match in
   order, skipping to-match component that is function-cache:dont-care")
  (:method ((cache hash-table-function-cache) cached-key to-match
            &key (test (let ((hash (cached-results cache)))
                         (when hash (hash-table-test hash)))))
    (when test
      (setf to-match (alexandria:ensure-list to-match))
      (iter
        (for k in cached-key)
        (for m = (or (pop to-match) 'dont-care))
        (unless (eql m 'dont-care)
          ;; TODO: should this recursivly call if k is a list?
          (always (funcall test k m)))
        (while to-match)))))

(defgeneric clear-cache-partial-arguments (cache to-match)
  (:documentation "This function will go through the cached-results removing
    keys that partially match the to-match list.

    This is used to clear the cache of shared? caches, but is also useful in
    other cases, where we need to clear cache for some subset of the
    arguments (eg: a cached funcall might wish to clear the cache of a
    specific funcalled function).

    Matches arguments for those provided. Anything not provided is considered
    function-cache:dont-care.  Anything specified as function-cache:dont-care
    is not used to determine if there is a match
   ")
  (:method ((cache hash-table-function-cache) to-match)
    (let* ((hash (cached-results cache))
           (test (when hash (hash-table-test hash))))
      (setf to-match (alexandria:ensure-list to-match))
      (iter (for (key value) in-hashtable hash)
        (when (partial-argument-match? cache key to-match :test test)
          (collect key into keys-to-rem))
        (finally (iter (for key in keys-to-rem)
                   (remhash key hash)))))))

(defmethod clear-cache ((cache hash-table-function-cache)
                        &optional (args nil args-input?)
                        &aux
                        (name (name cache))
                        (hash (cached-results cache))
                        (shared-results? (shared-results? cache)))
  (setf args (ensure-list args))
  ;; there was no cache, so there can be no results to clear
  (when hash
    (cond (args-input?
           (remhash (compute-cache-key cache args) hash))
          ((not shared-results?)
           ;; clear the whole hash, as they didnt specify args and
           ;; it doesnt share storage
           (clrhash hash))
          ;; we need to sort out which keys to remove based on our name
          (shared-results?
           (clear-cache-partial-arguments cache name)))))

(defmethod purge-cache ((cache hash-table-function-cache)
                        &aux (hash (cached-results cache)))
  (when hash
    (iter (for (key value) in-hashtable hash)
      (for (rtn . cached-at) = value)
      (when (expired? cache cached-at)
        (collect key into to-remove))
      (finally (iter (for rem in to-remove)
                 (remhash rem hash))))))

(defmethod reduce-cached-set ((cache hash-table-function-cache) n)
  ;; probably not super efficient and therefor probably likely to be a point
  ;; of slowdown in code we are trying to make fast with caching, an LRU/MRU
  ;; heap would be a better data structure for supporting this operation
  (iter
    (with ht = (cached-results cache))
    (for i from 0 to n)
    (for (key . val) in
         (sort
          (alexandria:hash-table-alist ht)
          #'<= :key #'cddr))
    (remhash key ht)))

(defclass hash-table-function-cache-with-capacity
    (cache-capacity-mixin hash-table-function-cache)
  ()
  (:documentation "a function cache that uses a hash-table to store results
  with a max capacity"))

(defclass metered-hash-table-cache (metered-mixin hash-table-function-cache)
  ()
  (:documentation "cache backed by a hash-table, keeps metrics"))
