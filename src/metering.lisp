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

(defmethod get-cached-value :around ((cache metered-mixin) cache-key)
  (multiple-value-bind (res at) (call-next-method)
    (if (or (null at) (expired? cache at))
        (incf (misses cache))
        (incf (hits cache)))
    (values res at)))

(defun incf-hash (key ht &optional (delta 1) (default 0))  
  (let* ((c (gethash key ht default)))
    (setf (gethash key ht) (+ c delta))))

(defun get-cached-object-stats
    (cache &aux
           (type-counts (make-hash-table))
           (cycles (make-hash-table))
           (res (cached-results cache)))
  (labels ((basic-type (v)
             (typecase v
               (null 'null)
               (string 'string)
               (number 'number)
               (cons 'cons)
               (hash-table 'hash-table)
               (vector 'vector)
               (standard-object 'standard-object)))
           (cycle-check? (v &aux (cnt (incf-hash v cycles 1 -1)))
             (when (= 1 cnt)
               (push v (gethash :cycle-objects  type-counts)))
             (plusp cnt))
           (categorize-value (v)
             (let* ((type (basic-type v)))
               (incf-hash type type-counts 1)
               (unless (or (member type '(number null))
                           (cycle-check? v))
                 (case type
                   (string
                    (incf-hash :string-length type-counts (length v)))
                   (cons      ;; handle cons cells and lists                  
                    (categorize-value (car v))
                    (when (cdr v) (categorize-value (cdr v))))
                   (vector
                    (map nil #'categorize-value v))
                   (hash-table
                    (incf-hash :hash-table-count type-counts (hash-table-count v))
                    (iter (for (k v) in-hashtable v)
                      (categorize-value v)))
                   (standard-object                    
                    (iter (for sd in (closer-mop:class-slots (class-of v)))
                      (for sn = (closer-mop:slot-definition-name sd))
                      (categorize-value (ignore-errors (slot-value v sn))))))))))
    (categorize-value res)
    (list*
     :cycles (iter (for (k v) in-hashtable cycles)
               (summing v))
     (alexandria:hash-table-plist type-counts))))

(defgeneric reset-counters (cache)
  (:documentation "When we clear the full cache, reset the counters")
  (:method ((cache symbol))
    (reset-counters (find-function-cache-for-name cache)))
  (:method ((cache metered-mixin))
    (setf (hits cache) 0
          (misses cache) 0)))
