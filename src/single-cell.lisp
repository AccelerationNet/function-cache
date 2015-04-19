(in-package :function-cache)

(defclass single-cell-function-cache (function-cache)
  ((test :accessor test :initarg :test :initform #'equal))
  (:documentation "a cache that stores only the most recent result of running
     the body"))

(defmethod make-cache-backing ((cache single-cell-function-cache))
  (cons nil (cons nil nil)))

(defmethod cached-results-count ((cache single-cell-function-cache))
  (if (cdr (cached-results cache)) 1 0))

(defmethod get-cached-value ((cache single-cell-function-cache) cache-key)
  (let* ((res (cached-results cache))
         (key (car res))
         (val (cadr res))
         (cached-at (cddr res)))
    (when (funcall (test cache) cache-key key)
      (values val cached-at))))

(defmethod (setf get-cached-value) (new (cache single-cell-function-cache) cache-key)
  (setf (cached-results cache)
        (cons cache-key (cons new *cached-at*)))
  new)

(defmethod purge-cache ((cache single-cell-function-cache))
  (let* ((res (cached-results cache))
         (cached-at (cddr res)))
    (when (expired? cache cached-at)
      (clear-cache cache))))

(defmethod key-cached? ((cache single-cell-function-cache) cache-key)
  (let ((key (car (cached-results cache))))
    (funcall (test cache) cache-key key)))
