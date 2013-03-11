(in-package :function-cache-test)

(defun run-all-tests (&optional (use-debugger t))
  (let ((lisp-unit:*print-failures* t)
        (lisp-unit:*print-errors* t)
        (lisp-unit::*use-debugger* use-debugger))
   (run-tests :all)))

(defvar *thunk-test-count* 0)
(defcached-thunk thunk-tester ()
  (incf *thunk-test-count*)
  7)

(define-test thunk-1
  (let ((*thunk-test-count* 0))
    (clear-cache *thunk-tester-cache*)
    (thunk-tester)
    (thunk-tester)
    (assert-eql 7 (thunk-tester))
    (assert-eql 1 *thunk-test-count*)))

(defvar *hash-test-count* 0)
(defcached fn0 (a0)
  (incf *hash-test-count*)
  a0)

(define-test fn0-test
  (let ((*hash-test-count* 0))
    (clear-cache *fn0-cache*)
    (fn0 1)
    (fn0 1)
    (assert-eql 1 (fn0 1))
    (assert-eql 1 *hash-test-count*)
    (fn0 2)
    (fn0 2)
    (assert-eql 2 (fn0 2))
    (assert-eql 2 *hash-test-count*)))

(defcached fn1 (a &key b c )
  (incf *hash-test-count*)
  (list a b c))

(define-test fn1-test
  (let ((*hash-test-count* 0))
    (clear-cache *fn1-cache*)
    (fn1 1 :b 2)
    (fn1 1 :b 2)
    (assert-equal '(1 2 nil) (fn1 1 :b 2))
    (assert-eql 1 *hash-test-count*)
    (fn1 2)
    (fn1 2 :c 3)
    (assert-equal '(1 2 3) (fn1 1 :b 2 :c 3))
    (assert-eql 4 *hash-test-count*)))


(progn
  (defparameter *shared-cache* (make-hash-table :test 'equal :synchronized t))
  (defparameter *shared-count* 0)
  (defparameter *shared0-count* 0)
  (defparameter *shared1-count* 0)
  (defcached-shared shared0-test (a &rest them)
    (incf *shared-count*)
    (incf *shared0-count*)
    (cons a them))

  (defcached-shared shared1-test (a &rest them)
    (incf *shared-count*)
    (incf *shared1-count*)
    (cons a them))

  (setf
   (cached-results *shared0-test-cache*) *shared-cache*
   (cached-results *shared1-test-cache*) *shared-cache*))

(define-test shared-test
  (let ((*shared-count* 0)
        (*shared0-count* 0)
        (*shared1-count* 0))
    (clear-cache *shared0-test-cache*)
    (clear-cache *shared1-test-cache*)
    (shared0-test 1 2 3)
    (shared0-test 1 2 3)
    (shared0-test 1 2 3)
    (assert-equal '(1 2 3) (shared0-test 1 2 3))
    (assert-eql 1 *shared0-count*)
    (assert-eql 0 *shared1-count*)
    (assert-eql 1 *shared-count*)
    (shared1-test 1 2 3)
    (shared1-test 1 2 3)
    (assert-equal '(1 2 3) (shared1-test 1 2 3))
    (assert-eql 1 *shared0-count*)
    (assert-eql 1 *shared1-count*)
    (assert-eql 2 *shared-count*)
    ))