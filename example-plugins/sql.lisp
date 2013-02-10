(in-package :query-fs)

(cffi:define-foreign-library libcrypt
  (t (:default "libcrypt")))
(cffi:use-foreign-library libcrypt)

(load-plugin "generic-readers.lisp")
(load-plugin "encrypt-file-content.lisp")
(require :clsql)

(defvar *db-connection-spec* (list "" "" "" ""))
(defvar *db-type* :postgresql)

(defmacro with-db (&body body)
  (let ((db-var (gensym)))
       `(clsql:with-database (,db-var *db-connection-spec* 
                             :pool t 
                             :database-type 
                             *db-type*)
                             (clsql:with-default-database 
                              (,db-var)
                              ,@body))))

(defun sql-real-value (x &key 
  (encoding cl-fuse-meta-fs::*meta-fs-name-encoding*))
  (cond
   ((stringp x) x)
   ((arrayp x) 
    (cl-fuse::octets-to-string x encoding)) 
   ((integerp x) (format nil "~a" x))
   (t x)))

(defun to-sql (x)
  (clsql:sql
   (sql-real-value x :encoding :direct)
   ))

(defun parse-sql-indexed (read-code)
       (cond
        ((and
          (listp read-code)
          (stringp (first read-code)))
         `(format nil ,(first read-code)
                  ,@(mapcar
                     (lambda (x) `(to-sql ,x))
                     (mapcar 'parse-indexed-code (cdr read-code)))))
        ((and
          (listp read-code)
          (>= (length read-code) 2)
          (symbolp (first read-code))
          (stringp (second read-code))
          (eq (first read-code) 'cached)
          )
          `(cached (format nil ,(second read-code)
                  ,@(mapcar
                     (lambda (x) `(to-sql ,x))
                     (mapcar 'parse-indexed-code (cddr read-code))))))
        (t (parse-indexed-code read-code))))

(def-reader sql-indexed ()
  (let ((read-code (read read-stream)))
       (parse-sql-indexed read-code)))

(def-reader sql-db-settings ()
  (let* ((kind (read read-stream))
         (source (read read-stream))
         (raw-data (read read-stream))
         (data
          (case source
                ((read)
                 (with-open-file 
                  (d raw-data) 
                  (read d)))
                ((set) raw-data)
                (t nil)
                ))
         )
        (case kind
              ((server) (setf (first *db-connection-spec*) data))
              ((name) (setf (second *db-connection-spec*) data))
              ((user) (setf (third *db-connection-spec*) data))
              ((password) (setf (fourth *db-connection-spec*) data))
              ((type) (setf *db-type* data))
              ((spec) (setf *db-connection-spec* data))
              (t nil))
        )
  t)

(defun decode-sql-strings (data &key (encoding :direct))
  (mapcar 
   (lambda(entry)
          (loop for x in entry
                collect
                (if (stringp x)
                    (cl-fuse::string-to-octets x encoding)
                    x)
                )) 
   data))

(def-reader sql-inner-query ()
  (let* ((query (sql-indexed-reader read-stream))
        (data
         (if (and
              (listp query)
              (eq (first query) 'cached)
              )
             (let ((query (second query)))
                  `(cached-expr
                    (with-db 
                     (decode-sql-strings (clsql:query ,query))
                     )
                    ,query
                    :eval-key t
                    :duration ,*object-cache-duration*
                    ))
             `(with-db 
               (decode-sql-strings (clsql:query ,query))
               ))
         ))
        data
       ))

(def-reader sql-with ()
  (let ((var (read read-stream)))
       `(mk-pair-generator ,var
                           (mapcar 
                            (lambda (e) 
                                    (list (sql-real-value (first e)) e))
                            ,@(progn (read read-stream) nil)
                            ,(sql-inner-query-reader read-stream))
                           (mk-dir (car ,var) :eval
                                   (let ((,var (cadr ,var)))
                                        (list
                                         ,@(collect-entries 
                                            read-stream *query-parser*))
                                        )))))

(def-reader sql-for ()
  (let ((var (read read-stream)))
       `(mk-pair-generator ,var 
                           (mapcar 
                            (lambda (e)
                                    (list (fmt "~a" 
                                               (sql-real-value (first e)))
                                          e))
                            ,@(progn (read read-stream) nil)
                            ,(sql-inner-query-reader read-stream))
                           (let* ((name 
                                   (cl-fuse::string-to-octets
                                    (first ,var)
                                    cl-fuse-meta-fs::*meta-fs-name-encoding*))
                                  (,var (second ,var)))
                                 (declare (ignorable ,var))
                                 ,(funcall *query-parser* read-stream)
                                 ))))

(def-reader sql-query-data ()
  (let ((var (read read-stream)))
       `(let ((,var ,(sql-inner-query-reader read-stream)))
             ,(funcall *query-parser* read-stream))))

(def-reader sql-syntax-toggler ()
  (let ((arg (read read-stream))) 
       (if (find arg '(t on 1))
           (clsql-sys:locally-enable-sql-reader-syntax)
           (clsql-sys:restore-sql-reader-syntax-state)
           ))
  t)

(def-reader sql-sink ()
  (let* ((var (read read-stream))
         (name (sql-indexed-reader read-stream))
         (getter (sql-indexed-reader read-stream))
         (setter (sql-indexed-reader read-stream))
         )
        `(mk-file 
          ,name
          ,getter
          (,var
           (let* ((,var 
                   (if (stringp ,var) ,var 
                       (cl-fuse::octets-to-string ,var :direct)))
                  )
                 (declare (ignorable ,var))
                 ,(if 
                   (and
                    (symbolp (first setter))
                    (fboundp (first setter))
                    (not (eq (first setter) 'format))
                    )
                   setter
                   `(progn
                     ;(format *error-output* "Writing to sink using query ~s~%" ,setter)
                     (with-db
                      (clsql:execute-command 
                       ,setter
                       ))))
                 t
                 ))
          )
        ))

(def-reader sql-sink-removable ()
  (let* ((var (read read-stream)))
        `(mk-file 
          ,(sql-indexed-reader read-stream)
          ,(sql-indexed-reader read-stream)
          (,var
           (let* ((,var 
                   (if (stringp ,var) ,var 
                       (cl-fuse::octets-to-string ,var :direct)))
                  )
                 (with-db
                  (clsql:execute-command 
                   ,(sql-indexed-reader read-stream)))
                 t
                 ))
          (progn
           (with-db
            (clsql:execute-command 
             ,(sql-indexed-reader read-stream)))
           t
           )
          )
        ))

(def-reader sql-symlink-removable ()
  `(mk-symlink
    ,(sql-indexed-reader read-stream)
    ,(sql-indexed-reader read-stream)
    (progn
     (with-db
      (clsql:execute-command 
       ,(sql-indexed-reader read-stream)))
     t
     )
    ))

(def-reader sql-creator ()
  (let* ((var (read read-stream)))
        `(mk-creator 
          ,var 
          (let
           ((,var (cl-fuse::recast-string 
                   ,var :full-range :direct)))
           (with-db
            (clsql:execute-command 
             ,(sql-indexed-reader read-stream))) 
           t)
          (progn ,var nil))))

(def-reader sql-dir-creator ()
  (let* ((var (read read-stream)))
        `(mk-creator 
          ,var (progn ,var nil)
          (let
           ((,var (cl-fuse::recast-string 
                   ,var :full-range :direct)))
           (with-db
            (clsql:execute-command 
             ,(sql-indexed-reader read-stream))) 
           t)
          )))

(def-reader sql-var ()
  (let ((var (read read-stream))
        (init-val (read read-stream))
        )
       (eval `(defvar ,var ,init-val))
       `(progn
         (defvar ,var ,init-val)
         nil
         )))

(def-linear-query-parser "sql"
  ((db) sql-db-settings-reader)
  ((local-db) preserve-vars-reader '(*db-connection-spec* *db-type*))
  ((lisp-escape) lisp-escape-reader)
  ((file) file-reader)
  ((symlink) symlink-reader)
  ((symlink-removable) sql-symlink-removable-reader)
  ((dir) dir-reader)
  ((with) sql-with-reader)
  ((list) direct-list-reader)
  ((for) sql-for-reader)
  ((syntax) sql-syntax-toggler-reader)
  ((sink) sql-sink-reader)
  ((sink-removable) sql-sink-removable-reader)
  ((create) sql-creator-reader)
  ((create-dir) sql-dir-creator-reader)
  ((query-data) sql-query-data-reader)
  ((transient) sql-var-reader)
  ((encrypted) encrypted-file-reader (lambda (x) `(sql-real-value ,x :encoding :direct)))
  ((proxy) proxy-reader)
  ((setter) setter-reader)
  ((end nil :eof) nil)
  (t nil))
