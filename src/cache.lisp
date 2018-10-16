(in-package :function-cache)
(cl-interpol:enable-interpol-syntax)

(defvar *cache-names* nil "A list of all function-caches")
(defvar *default-cache-class* 'metered-hash-table-cache
  "cache class to use if unspecified and thunk is not applicable")

(defun do-caches (fn &key package)
  "Iterate through caches calling fn on each matching cache"
  (when package (setf package (find-package package)))
  (iter (for n in *cache-names*)
    (when (or (null package) (eql (symbol-package n) package))
      (funcall fn (symbol-value n)))))

(defun find-function-cache-for-name (cache-name)
  "given a name get the cache object associated with it"
  (iter (for name in *cache-names*)
    (for obj = (symbol-value name))
    (when (or (eql name cache-name) ;; check the cache name
              (eql (name obj) cache-name)) ;; check the fn name
      (return obj))))

(defmethod clear-cache ((cache-name null) &optional args)
  (declare (ignore cache-name args))
  nil)

(defmethod clear-cache ((cache-name symbol) &optional (args nil args-input?))
  (let ((obj (find-function-cache-for-name cache-name)))
    (unless obj
      (error "Couldnt find cache with name ~A" cache-name))
    ;; only call with args if we called this with args
    ;; otherwise there is no determination between (eg: &rest called with nil args
    ;; and not calling with args)
    (if args-input?
        (clear-cache obj args)
        (clear-cache obj))))

(defmethod purge-cache ((cache-name symbol))
  (purge-cache (find-function-cache-for-name cache-name)))

(defun clear-cache-all-function-caches (&optional package)
  "Clear all the packages we know about. If there is a package mentioned,
   clear only those caches whose names are in that package"
  (do-caches #'clear-cache :package package))

(defun purge-all-caches (&optional package)
  "Call purge on all matching cache objects.  If package is provided, purge
   only caches located within that package"
  (do-caches #'purge-cache :package package))


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
        (t *default-cache-class*)))))

(defun %call-list-for-lambda-list (lambda-list)
  "Turns a lambda list into a list that can be applied to functions of that lambda list"
  (multiple-value-bind (args optional rest keys)
      (alexandria:parse-ordinary-lambda-list lambda-list)
    (let* ((call-list (append args
                              (mapcar #'first optional)
                              (mapcan #'first keys)
                              ))
           (call-list (cond
                        ((and call-list rest)
                         `(list* ,@call-list ,rest))
                        (call-list `(list ,@call-list))
                        (rest rest))))
      call-list)))

(defmacro defcached (symbol lambda-list &body body)
  "Creates a cached function named SYMBOL and a cache object named *{FN-NAME}-CACHE*
   SYMBOL can also be a list (FN-NAME &rest CACHE-INIT-ARGS
                           &key CACHE-CLASS TABLE TIMEOUT SHARED-RESULTS?)

   TABLE - a shared cache-store to use, usually a hash-table, a function that returns
     a hashtable, or a symbol whose value is a hash-table
   TIMEOUT - how long entries in the cache should be considered valid for
   CACHE-CLASS - controls what cache class will be instantiated (uses
     default-cache-class if not provided)
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
           (call-list (%call-list-for-lambda-list lambda-list))
           doc declares)

      (when (stringp (first body))
        (setf doc (first body)
              body (rest body)))
      (iter (while (and (listp (first body))
                        (eql 'declare (first (first body)))))
            (collect (first body) into decs)
            (setf body (rest body))
            (finally (setf declares decs)))

      `(progn
        (defvar ,cache nil)
        (pushnew ',cache *cache-names*)
        (defun ,fn-name ,lambda-list
          ,@(ensure-list doc)
          (cacher ,cache ,call-list))
        (setf ,cache
         (make-instance ',cache-class
          :body-fn (lambda ,lambda-list
                     ,@(ensure-list doc)
                     ,@declares
                     (block ,fn-name
                       (block nil ,@body)))
          :name ',fn-name
          :lambda-list ',lambda-list
          :shared-results? ,shared-results?
          :cached-results ,table
          ,@cache-args))))))
