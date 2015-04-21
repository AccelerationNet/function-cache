(in-package :function-cache)

(defclass thunk-cache (function-cache)
  ()
  (:documentation "a cache optimized for functions of no arguments
     (uses a cons for caching)"))

(defmethod get-cached-value ((cache thunk-cache) cache-key)
  (declare (ignore cache-key))
  (let* ((res (car (cached-results cache)))
         (cached-at (cdr (cached-results cache))))
    (values res cached-at)))

(defmethod (setf get-cached-value) (new (cache thunk-cache) cache-key)
  (declare (ignore cache-key))
  (setf (cached-results cache)
        (cons new *cached-at*))
  new)

(defmethod cached-results-count ((cache thunk-cache))
  (if (cdr (cached-results cache)) 1 0))

(defmethod purge-cache ((cache thunk-cache))
  (let ((cached-at (cdr (cached-results cache))))
    (when (expired? cache cached-at)
      (clear-cache cache))))

(defmethod key-cached? ((cache thunk-cache) cache-key)
  (declare (ignore cache-key))
  (cdr (cached-results cache)))
