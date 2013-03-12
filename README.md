# Function cache 

A common lisp library that provides extensible function result caching
based on arguments (memoization).

## Differences from fare-memoization

 * By default uses separate tables for each function
 * Supports other cache structures (eg: optimizations-for-thunks, mru-heaps)
