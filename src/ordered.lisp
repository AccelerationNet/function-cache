(in-package :function-cache)

(deftype cache-node () '(or null cnode))
(defstruct (cnode
             (:constructor cnode (cache-key result &optional older newer)))
  cache-key
  result
  (older nil :type cache-node)
  (newer nil :type cache-node))


(defclass ordered-cache-mixin (cache-capacity-mixin)
  ((oldest :initform nil :type cache-node :accessor oldest-node)
   (newest :initform nil :type cache-node :accessor newest-node))
  (:documentation "Mixin that keeps track of the order of cached results in a doubly linked list.
FIRST references the oldest cached result, and LAST references the most recent."))

(defun %add-cached-node (cache node)
  (declare (ordered-cache-mixin cache) (cnode node))
  "Add a node to the last position of the cache."
  (with-slots (oldest newest) cache
    (etypecase newest
      (cnode (setf (cnode-newer newest) node)
             (setf (cnode-older node) newest)
             (setf newest node))
      (null (setf newest node)
            (setf oldest node))))
  node)

(defun %remove-cached-node (cache node)
  (declare (ordered-cache-mixin cache) (cnode node))
  "Remove a node from the cache."
  (with-slots (oldest newest) cache
    (let ((newer (cnode-newer node))
          (older (cnode-older node)))
      (if older
          (setf (cnode-newer older) newer)
          (setf oldest newer))
      (if newer
          (setf (cnode-older newer) older)
          (setf newest older))))
  node)

(defun %move-cached-node (cache node)
  (declare (ordered-cache-mixin cache) (cnode node))
  "Move a node to the end of the cache, should be called when a cached result has been used."
  (%remove-cached-node cache node)
  (setf (cnode-newer node) nil)
  (setf (cnode-older node) nil)
  (%add-cached-node cache node))

(defmethod get-cached-value :around ((cache ordered-cache-mixin) cache-key)
  (multiple-value-bind (result-node cached-at) (call-next-method)
    (if result-node
        (progn
          (%move-cached-node cache result-node)  ; Move the result to the end if there was a cached result.
          (values (cnode-result result-node) cached-at))
        (values nil nil))))

(defmethod (setf get-cached-value) :around (new (cache ordered-cache-mixin) cache-key)
  (let ((node (cnode cache-key new)))
    (call-next-method node cache cache-key)
    (%add-cached-node cache node)))

(defun sync-ordered-cache (cache)
  (declare (ordered-cache-mixin cache))
  "Remove any nodes from the dlist that are no longer in the actual cache."
  (iter (for node first (oldest-node cache) then (cnode-newer node))
        (while node)
        (for key = (cnode-cache-key node))
        (unless (key-cached? cache key)
          (%remove-cached-node cache node))))

(defmethod clear-cache-partial-arguments :after ((cache ordered-cache-mixin) to-match)
  (sync-ordered-cache cache))

(defmethod clear-cache :after ((cache ordered-cache-mixin) &optional args)
  (declare (ignore args))
  (sync-ordered-cache cache))

(defmethod purge-cache :after ((cache ordered-cache-mixin))
  (sync-ordered-cache cache))

(defclass lru-cache (ordered-cache-mixin hash-table-function-cache)
  ()
  (:documentation "LRU cache backed by a hash-table.
Maintains capacity by removing least recently used cached values."))

(defmethod reduce-cached-set ((cache lru-cache) n)
  (iter
   (with ht = (cached-results cache))
   (for i from 0 to n)
   (for node first (oldest-node cache) then (cnode-newer node))
   (while node)
   (remhash (cnode-cache-key node) ht)
   (%remove-cached-node cache node)))

(defclass mru-cache (ordered-cache-mixin hash-table-function-cache)
  ()
  (:documentation "MRU cache backed by a hash-table.
Maintains capacity by removing the most recently used cached value.s"))

(defmethod reduce-cached-set ((cache mru-cache) n)
  (iter
    (with ht = (cached-results cache))
    (for i from 0 below n)
    (for node first (newest-node cache) then (cnode-older node))
    (while node)
    (remhash (cnode-cache-key node) ht)
    (%remove-cached-node cache node)))
