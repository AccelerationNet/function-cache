(in-package :function-cache)
(cl-interpol:enable-interpol-syntax)

(defclass function-cache ()
  ((cached-results :accessor cached-results :initform nil :initarg
                   :cached-results)
   (timeout :accessor timeout :initform nil :initarg :timeout)
   (body-fn :accessor body-fn :initform nil :initarg :body-fn)
   (name :accessor name :initform nil :initarg :name)
   (lambda-list :accessor lambda-list :initform nil :initarg :lambda-list))
  (:documentation "an object that contains the cached results of function calls
    the original function to be run, to set cached values
    and other cache configuration parameters.  This class is mostly intended
    to be abstract with hash-table-function-cache, and thunk-cache being the
    current concrete classes"))

(defmethod cached-results :around ((cache function-cache))
  "Coerce the refernce to the results into something we can use"
  (let ((result (call-next-method)))
    (typecase result
      (null nil)
      (function (funcall result))
      (symbol (cond ((boundp result) (symbol-value result))
                    ((fboundp result) (funcall result))))
      (t result))))

(defclass thunk-cache (function-cache)
  ()
  (:documentation "a cache optimized for functions of no arguments
     (uses a cons for caching)"))

(defclass single-cell-function-cache (function-cache)
  ((test :accessor test :initarg :test :initform #'equal))
  (:documentation "a cache that stores only the most recent result of running
     the body"))

(defclass hash-table-function-cache (function-cache)
  ((hash-init-args :accessor hash-init-args :initform
                   '(:test equal :synchronized t) :initarg :hash-init-args)
   (shared-results? :accessor shared-results? :initform nil :initarg
                    :shared-results?))
  (:documentation "a function cache that uses a hash-table to store results"))

(defmethod initialize-instance :after
    ((cache function-cache) &key &allow-other-keys)
  (ensure-cache-backing cache))

(defgeneric ensure-cache-backing (cache)
  (:documentation "ensures that cached-results has the expected init value")
  (:method ((cache function-cache)) t)
  (:method ((cache single-cell-function-cache))
    (unless (slot-value cache 'cached-results)
      (setf (slot-value cache 'cached-results)
            (cons nil (cons nil nil)))))
  (:method ((cache hash-table-function-cache))
    (unless (slot-value cache 'cached-results)
      (setf (slot-value cache 'cached-results)
            (apply #'make-hash-table (hash-init-args cache))))))

(defgeneric expired? ( cache result-timeout )
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
             (< expires-at (get-universal-time))))
        ))))

(defgeneric get-cached-value (cache cache-key)
  (:documentation "returns the result-values-list and at what time it was cached")
  (:method ((cache hash-table-function-cache) cache-key)
    ;; if we get no hash when we expect one then it probably means
    ;; that we should just run tthe body (eg: http-context cached results
    ;; a valid http context)
    (let* ((hash (cached-results cache))
           (cons (when hash (gethash cache-key (cached-results cache))))
           (res (car cons))
           (cached-at (cdr cons)))
      (values res cached-at)))
  (:method ((cache thunk-cache) cache-key)
    (declare (ignore cache-key))
    (let* ((res (car (cached-results cache)))
           (cached-at (cdr (cached-results cache))))
      (values res cached-at)))
  (:method ((cache single-cell-function-cache) cache-key)
    (let* ((res (cached-results cache))
           (key (car res))
           (val (cadr res))
           (cached-at (cddr res)))
      (when (funcall (test cache) cache-key key)
        (values val cached-at)))))

(defgeneric (setf get-cached-value) (new cache cache-key)
  (:documentation "Set the cached value for the cache key")
  (:method (new (cache single-cell-function-cache) cache-key)
    (setf (cached-results cache)
          (cons cache-key (cons new (get-universal-time)))))
  (:method (new (cache hash-table-function-cache) cache-key)
    ;; without our shared hash, we cannot cache
    (let ((hash (cached-results cache)))
      (when hash
        (setf (gethash cache-key hash)
              (cons new (get-universal-time))))))
  (:method (new (cache thunk-cache) cache-key)
    (declare (ignore cache-key))
    (setf (cached-results cache)
          (cons new (get-universal-time)))))

(defgeneric defcached-hashkey (thing)
  (:documentation "Turns a list of arguments into a valid cache-key
    (usually a tree of primatives)")
  (:method ((thing T))
    (typecase thing
      (null nil)
      (list (iter (for i in thing)
              (collect (defcached-hashkey i))))
      (t thing))))

(defgeneric compute-cashe-key (cache thing)
  (:documentation "Used to assemble cache keys for function-cache objects")
  (:method ((cache function-cache) thing)
    (defcached-hashkey thing))
  (:method ((cache hash-table-function-cache) thing)
    (let ((rest (ensure-list (defcached-hashkey thing))))
      (if (shared-results? cache)
          (list* (name cache) rest)
          rest))))

(defgeneric cacher (cache args)
  (:documentation "A function that takes a cache object and an arg list
    and either runs the computation and fills the caches or retrieves
    the cached value")
  (:method ((cache function-cache) args
            &aux (cache-key (compute-cashe-key cache args)))
    (multiple-value-bind (cached-res cached-at)
        (get-cached-value cache cache-key)
      (if (or (null cached-at) (expired? cache cached-at))
          (let ((results (multiple-value-list (apply (body-fn cache) args))))
            (setf (get-cached-value cache cache-key) results)
            (apply #'values results))
          (apply #'values cached-res)))))

(defvar *cache-names* nil
  "A list of all function-caches")

(defgeneric clear-cache (cache &optional args)
  (:documentation "Clears a given cache")
  (:method ((cache symbol) &optional args)
    (clear-cache (find cache *cache-names* :key #'name) args))
  (:method ((cache function-cache) &optional args)
    (declare (ignore args))
    (setf (cached-results cache) nil))
  (:method ((cache hash-table-function-cache)
            &optional (args nil args-input?)
            &aux
            (name (name cache))
            (hash (cached-results cache))
            (shared-results? (shared-results? cache)))
    (setf args (ensure-list args))
    ;; there was no cache, so there can be no results to clear
    (when hash
      (cond (args-input?
             (remhash (compute-cashe-key cache args) hash))
            ((not shared-results?)
             ;; clear the whole hash, as they didnt specify args and
             ;; it doesnt share storage
             (clrhash hash))
            ;; we need to sort out which keys to remove based on our name
            (shared-results?
             (iter (for (key value) in-hashtable hash)
               (when (eql name (first key))
                 (collect key into keys-to-rem))
               (finally (iter (for key in keys-to-rem)
                          (remhash key hash)))))))))

(defun clear-cache-all-function-caches (&optional package)
  (when package (setf package (find-package package)))
  (iter (for n in *cache-names*)
    (when (or (null package)
              (eql (symbol-package n) package))
      (clear-cache (symbol-value n)))))

(defun %ensure-unquoted (thing)
  (etypecase thing
    (null nil)
    (symbol thing)
    (list (when (eql 'quote (first thing))
            (second thing)))))

(defgeneric default-cache-class (symbol lambda-list)
  (:documentation "A function that takes symbol lambda-list and perhaps a cache-class")
  (:method (symbol lambda-list)
    (destructuring-bind (fn-name &key cache-class &allow-other-keys)
        (ensure-list symbol)
      (declare (ignore fn-name))
      (setf cache-class (%ensure-unquoted cache-class))
      (cond
        (cache-class cache-class)
        ((null lambda-list) 'thunk-cache)
        (t 'hash-table-function-cache)))))

(defun %call-list-for-lambda-list (lambda-list)
  (multiple-value-bind (args optional rest keys)
      (alexandria:parse-ordinary-lambda-list lambda-list)
    (let* ((call-list (append args
                              (mapcar #'first optional)
                              (mapcan #'first keys)
                              ))
           (call-list (if rest
                          `(list* ,@call-list ,rest)
                          `(list ,@call-list))))
      call-list)))

(defmacro defcached (symbol lambda-list &body body)
  "Creates a cached function named SYMBOL and a cache object named *{FN-NAME}-CACHE*
   SYMBOL can also be a list (FN-NAME &rest cache-init-args
                           &key CACHE-CLASS TABLE TIMEOUT SHARED-RESULTS?)

   CACHE-CLASS - controls what cache class will be instantiated (uses
     default-cache-class if not provided)
   TABLE - a shared cache-store to use, usually a hash-table, a function that returns
     a hashtable, or a symbol whose value is a hash-table
   TIMEOUT - how long entries in the cache should be considered valid for
   SHARED-RESULTS? - do we expect that we are sharing cache space with other things
     defaults to t if TABLE is provided
   CACHE-INIT-ARGS - any other args that should be passed to the cache
  "
  (destructuring-bind (fn-name
                       &rest cache-args
                       &key table (shared-results? nil shared-result-input?)
                       cache-class
                       &allow-other-keys)
      (ensure-list symbol)
    (declare (ignore cache-class));; handled in default-cache-class
    (remf cache-args :cache-class)
    (remf cache-args :table)
    (remf cache-args :shared-results?)
    (when (and table (not shared-result-input?))  (setf shared-results? t))
    (let* ((cache-class (default-cache-class symbol lambda-list))
           (cache (symbol-munger:english->lisp-symbol #?"*${ fn-name }-cache*"))
           (doc (when (stringp (first body)) (first body)))
           (call-list (%call-list-for-lambda-list lambda-list)))
      `(progn
        (defvar ,cache nil)
        (pushnew ',cache *cache-names*)
        (setf ,cache
         (make-instance ',cache-class
          :body-fn (lambda ,lambda-list ,@body)
          :name ',fn-name
          :lambda-list ',lambda-list
          :shared-results? ,shared-results?
          :cached-results ,table
          ,@cache-args))
        (defun ,fn-name ,lambda-list
          ,doc
          (cacher ,cache ,call-list))))))
