(in-package :function-cache)
(cl-interpol:enable-interpol-syntax)

;;;; TODO, ideas: Purger threads, MRU heap cache,
;;;; refreshable caches (need to store actual args as well (instead of just
;;;; cache key), which has storage implications)

(defclass function-cache ()
  ((cached-results :accessor cached-results :initform nil :initarg
                   :cached-results)
   (timeout :accessor timeout :initform nil :initarg :timeout)
   (body-fn :accessor body-fn :initform nil :initarg :body-fn)
   (name :accessor name :initform nil :initarg :name)
   (lambda-list :accessor lambda-list :initform nil :initarg :lambda-list)
   (shared-results? :accessor shared-results? :initform nil :initarg
                    :shared-results?)
   )
  (:documentation "an object that contains the cached results of function calls
    the original function to be run, to set cached values
    and other cache configuration parameters.  This class is mostly intended
    to be abstract with hash-table-function-cache, and thunk-cache being the
    current concrete classes"))

(defmethod print-object ((o function-cache) s)
  (print-unreadable-object (o s :type t :identity t)
    (ignore-errors
     (iter (for c in '(name))
       (for v = (ignore-errors (funcall c o)))
       (when v (format s "~A:~S " c v))))))

(defmethod cached-results :around ((cache function-cache))
  "Coerce the refernce to the results into something we can use"
  (let ((result (call-next-method)))
    (typecase result
      (null nil)
      (function (funcall result))
      (symbol (cond ((boundp result) (symbol-value result))
                    ((fboundp result) (funcall result))))
      (t result))))

(defmethod initialize-instance :after ((cache function-cache) &key &allow-other-keys)
  (unless (slot-value cache 'cached-results)
    (setf (slot-value cache 'cached-results)
          (make-cache-backing cache))))

; is this needed?
;(defmethod cached-results-count ((cache function-cache))
;  (cached-results-count (cached-results cache)))

(defmethod clear-cache ((cache function-cache) &optional args)
  (declare (ignore args))
  (setf (cached-results cache) nil))

(defmethod purge-cache :around ((cache function-cache))
  ;; only actually purge if there is the possibility of removing entries
  (when (timeout cache) (call-next-method)))

(defgeneric compute-cache-key (cache thing)
  (:documentation "Used to assemble cache keys for function-cache objects")
  (:method ((cache function-cache) thing)
    (let ((rest (ensure-list (defcached-hashkey thing))))
      (if (shared-results? cache)
          (list* (name cache) rest)
          rest))))

(defun %insert-into-cache (cache args &key (cache-key (compute-cache-key cache args)))
  "Simple helper to run the body, store the results in the cache and then return them"
  (let ((results (multiple-value-list (apply (body-fn cache) args))))
    (setf (get-cached-value cache cache-key) results)
    (apply #'values results)))

(defvar *bypass-cache* nil "if non-nil, skip any kind of caching")

(defgeneric cacher (cache args)
  (:documentation "A function that takes a cache object and an arg list
    and either runs the computation and fills the caches or retrieves
    the cached value")
  (:method ((cache function-cache) args
            &aux (cache-key (compute-cache-key cache args)))
    (if *bypass-cache*
        (apply (body-fn cache) args)
        (multiple-value-bind (cached-res cached-at)
            (get-cached-value cache cache-key)
          (cond
            ((null cached-at)
             (%insert-into-cache cache args))
            ((expired? cache cached-at)
             (with-simple-restart (abort "Skip expiring this value")
               (signal 'expired-a-value
                       :cache cache :key cache-key :value cached-res
                       :cached-at cached-at)
               (%insert-into-cache cache args)))
            (t (apply #'values cached-res)))))))
