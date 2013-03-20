(in-package :function-cache)

(defmethod function-cache:defcached-hashkey ((attr clsql-sys:sql-ident-attribute))
  (clsql-sys:sql attr))

(defmethod function-cache:defcached-hashkey ((attr clsql-sys::%sql-expression))
  (clsql-sys:sql attr))

(defmethod function-cache:defcached-hashkey ((o clsql-sys:standard-db-object))
  (clsql-sys:sql (clsql-helper:primary-key-where-clauses o)))