(in-package :function-cache)

;;;
;;; calculate metrics for caches
;;;

(defclass metered-mixin ()
  ((hits :accessor hits :initform 0
         :documentation "how many cache hits")
   (misses :accessor misses :initform 0
           :documentation "how many cache hits"))
  (:documentation "adds some recording for caches hits/misses"))

(defun accesses (cache)
  "how many total accesses this cache has seen"
  (+ (hits cache) (misses cache)))

(defun hit-ratio (cache)
  (/ (hits cache) (accesses cache)))

(defmethod clear-cache ((cache metered-mixin) &optional args)
  (declare (ignore args))
  (setf (hits cache) 0
        (misses cache) 0)
  (call-next-method))

(defmethod get-cached-value :around ((cache metered-mixin) cache-key)
  (multiple-value-bind (res at) (call-next-method)
    (if (or (null at) (expired? cache at))
        (incf (misses cache))
        (incf (hits cache)))
    (values res at)))

