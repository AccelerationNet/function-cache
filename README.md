# function-cache

A common lisp library that provides extensible function result caching
based on arguments (an expanded form of memoization).

## Differences from fare-memoization

 * By default uses separate tables for each function
 * Supports other cache structures (eg: optimization for thunks, mru-heaps)
 * Supports optional caching for functions that are called in transient
   cache contexts (eg: http-context, web application session etc)
 * Supports timeouts to invalidate caches, and purging of expired
   cached results
 * A more robust cache clearing scheme, clear all caches, all caches
   in a package, or just a specific cache

## API

### DEFCACHED

Creates a cached function named SYMBOL and a cache object named `*{FN-NAME}-CACHE*`
SYMBOL can also be a list

`(FN-NAME &rest cache-init-args
   &key CACHE-CLASS TABLE TIMEOUT SHARED-RESULTS?)`

 * CACHE-CLASS - controls what cache class will be instantiated (uses
   default-cache-class if not provided)
 * TABLE - a shared cache-store to use, usually a hash-table, a function that returns
   a hashtable, or a symbol whose value is a hash-table
 * TIMEOUT - how long entries in the cache should be considered valid for, in seconds
 * SHARED-RESULTS? - do we expect that we are sharing cache space with other things
   defaults to t if TABLE is provided

```lisp
(defcached symbol (lambda-list) ...)
(defcached (symbol &key table cache-class timeout shared-results?) (lambda-list) ...)
EG:

(defcached (foo :timeout 10) (arg0)
  (sleep 3)
  arg0)
(foo 1) ;; waits 3s then returns 1
(foo 1) ;; returns 1 immediately
(foo 0) ;; waits 3s then returns 0
(foo 0) ;; returns 0 immediately
(foo 2) ;; waits 3s then returns 2
(foo 3) ;; waits 3s then returns 3
(foo 1) ;; has timedout, waits 3s then returns 1
```

#### Cache-objects and Names

Each cached function will have a cache object associated with it in
special-variable `*{FN-NAME}-CACHE*`.  You can find a cache object for
a function name using `find-function-cache-for-name` if necessary.  In
most of the api, a function-name or a cache-object should be
interchangable.

### function-cache, thunk-cache, hash-table-function-cache, single-cell-function-cache

The basic types of function-caches currently provided.
 * Function-cache - abstract class for all function-caches
 * thunk-cache - a cache specialized for storing the results of thunks
  * automatically chosen, for functions of 0 args
 * hash-table-function-cache
  * A hash-table backed function-cache, supports shared hash-tables, but defaults
    to a unique hash-table for each cache
  * Supports dynamically available caching (eg: web-request contexts),
    by setting cache-results to a function or symbol that will
    possibly return a hashtable. If either of these return nil, then
    caching is bypassed for that call
 * single-cell-function-cache
  * caches the single most recently used call
 * lru-cache
  * A cache with a fixed capacity
  * Backed by a combination of a hash table and doubly-linked list
  * When the maximimum capacity is reached the least recently used cache
    entries are deleted.
 * mru-cache
  * A cache with a fixed capacity
  * Backed by a combination of a hash table and doubly-linked list
  * When the maximum capacity is reached the most recently used cache
    entries are deleted.

### get-cached-value, (setf get-cached-value)

One of the main expansion points for new cache subclasses. These place
and retrieve values from whatever backing store the cache is using

Returns (values results cached-at) and stores the results of executing
the underlying function.

### purge-cache, purge-all-caches

A function that will remove expired entries from the cache, allowing
them to be garbage collected

### clear-cache, clear-all-caches

A function that will remove all cached results from the cache.

`clear-cache` accepts either a cache or the name of a cached function

#### clear-cache-partial-arguments

This function will go through the cached-results removing
keys that partially match the to-match list.

This is used to clear the cache of shared? caches, but is also useful
in other cases, where we need to clear cache for some subset of the
arguments (eg: a cached funcall might wish to clear the cache of a
specific funcalled function).

Matches arguments for those provided. Anything not provided is
considered function-cache:dont-care.  Anything specified as
function-cache:dont-care is not used to determine if there is a match

uses partial-argument-match? to determine if the key should be removed

#### partial-argument-match?
Trys to see if the cache-key matches the to-match partial
key passed in.

The basic implementation is to go through the cache-keys and match in
order, skipping to-match component that is function-cache:dont-care

### compute-cache-key, defcached-hashkey

Compute-cache-key, takes a cache and a list of arguments and turns
those into a valid cache-key for the cache, by calling
defcached-hashkey recursively through the argument tree.

Shared cache-backings will ensure the function name is the first token
in this cache-key

### cacher

Cacher is responsible for looking up memoized results in the cache,
checking if they are expired/missing and running the body if so,
caching and returning the result

### \*cache-names\*

A list of all the special variables created by defcached.  Used to
ease clearing/purging all the caches, and for introspective purposes.


## Authors

 * [Acceleration.net](http://www.acceleration.net/)
  * [Russ Tyndall](http://russ.unwashedmeme.com/blog)
  * [Nathan Bird](http://the.unwashedmeme.com/blog)
  * [Ryan Davis](http://ryepup.unwashedmeme.com/blog)

```
;; Copyright (c) 2013 Russ Tyndall , Acceleration.net http://www.acceleration.net
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are
;; met:
;;
;;  - Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;;
;;  - Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;; A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT
;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
```
