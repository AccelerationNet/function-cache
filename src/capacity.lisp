(in-package :function-cache)

(defclass cache-capacity-mixin ()
  ((capacity
    :accessor capacity :initarg :capacity :initform nil
    :documentation "The maximum number of objects cached, when we hit this we
    will reduce the number of cached entries by reduce-by-ratio")
   (reduce-by-ratio
    :accessor reduce-by-ratio :initarg :reduce-by-ratio :initform .2
    :documentation "Remove the oldest reduce-by-ratio entries (eg: .2 or 20%)")))

(defun number-to-remove (cache)
  (ceiling (* (capacity cache) (reduce-by-ratio cache))))

(defmethod at-cache-capacity? ((cache cache-capacity-mixin))
  (and (capacity cache)
       (>= (cached-results-count cache) (capacity cache))))

(defmethod (setf get-cached-value) :before (new (cache cache-capacity-mixin) cache-key)
  (when (at-cache-capacity? cache)
    (reduce-cached-set cache (number-to-remove cache))))
