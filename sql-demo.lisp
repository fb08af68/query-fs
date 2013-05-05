; Query-FS
; Based on cl-fuse FUSE bindings for Common Lisp
; Distributed as free software under Lisp Library General Public License
; Distributed AS IS with NO WARRANTY

(in-package :query-fs)

(defmacro with-db (&body body)
  (let ((db-var (gensym)))
    `(clsql:with-database 
       (,db-var *db-connection-spec* 
		:pool t 
		:database-type 
		*db-type*)
       (clsql:with-default-database 
	 (,db-var)
	 ,@body))))
(defvar *db-connection-spec* (list "" "" "" "" nil))
(defvar *db-type* :postgresql)

(defmacro 
  read-variable (prompt var default)
  `(progn
     (format t "~a [empty for ~s]: " ,prompt ,default)
     (force-output)
     (setf ,var (string-trim " " (read-line)))
     (when (equal ,var "")
       (setf ,var ,default))))

(defun read-stream-contents (f)
  (let* ((out-data 
	   (make-array 
	     (file-length f) 
	     :element-type 'character
	     :initial-element #\Space)))
    (read-sequence out-data f)
    out-data
    ))

(defun
  queryfs-sql-demo ()
  (let*
    (server-name database-name database-type database-user database-password
		 table-name name-column attribute-1-column attribute-2-column
		 fill-random-data target-directory source-directory
		 skip-preparation skip-launch)

    (format 
      t 
      "Please note that queries and plugins in demo target directory may be overwritten~%")
    (read-variable 
      "Please specify the target directory for FS mounting demo"
      target-directory "/tmp/query-fs-test/")

    (read-variable
      "Please specify whether to skip preparations" skip-preparation "no")
    (case (elt (string-downcase skip-preparation) 0)
      ((#\y #\t #\1) (setf skip-preparation t))
      ((#\n #\f #\0) (setf skip-preparation nil))
      (t (error "Answer not understood")))

    (unless skip-preparation
      (read-variable 
	"Please specify your database type in CLSQL notation"
	database-type "PostgreSQL-socket")
      (setf database-type (intern (string-upcase database-type) :keyword))
      (read-variable "Please specify database server name" server-name "localhost")
      (read-variable "Please specify database connection user name" database-user (getenv "USER"))
      (read-variable "Please specify database connection password (password is echoed)"
		     database-password "")
      (read-variable "Please specify database name" database-name "queryfs_test")

      (setf *db-connection-spec* 
	    (list server-name database-name database-user database-password))
      (setf *db-type* (intern (string-upcase database-type) :keyword))

      (progn
	(format t "Now testing database connection...~%")
	(with-db (or (clsql:connected-databases) (clsql:reconnect)))
	(format t "Looks like a success.~%"))

      (format t "Table can be created and/or filled with random data if you so prefer.~%")

      (read-variable "Please specify table name" table-name "queryfs_test")
      (read-variable "Please specify unique caption column" name-column "name")
      (read-variable "Please specify first attribute column" attribute-1-column "kind")
      (read-variable "Please specify second attribute column" attribute-2-column "comment")

      (setf table-name (clsql:sql-expression 
			 :table (intern (string-upcase table-name) :keyword)))
      (setf 
	name-column
	(clsql:sql-expression
	  :attribute (intern (string-upcase name-column) :keyword)))
      (setf 
	attribute-1-column
	(clsql:sql-expression
	  :attribute (intern (string-upcase attribute-1-column) :keyword)))
      (setf 
	attribute-2-column
	(clsql:sql-expression
	  :attribute (intern (string-upcase attribute-2-column) :keyword)))

      (if
	(ignore-errors (with-db (clsql:select 1 :from table-name :limit 1)) t)
	(progn
	  (format t "Table found~%")
	  )
	(progn
	  (format t "Table doesn't exist. Creating.~%")
	  (with-db
	    (clsql:create-table
	      table-name
	      (list
		(list name-column 'varchar 'unique)
		(list attribute-1-column 'varchar)
		(list attribute-2-column 'varchar)
		)))
	  (format t "Done.~%")
	  ))

      (progn
	(format t "Checking whether the needed columns are present...~%")
	(with-db (clsql:select name-column attribute-1-column attribute-2-column
			       :from table-name :limit 1))
	(format t "OK.~%"))

      (read-variable "Do you want demo strings to be added to the table" 
		     fill-random-data "no")
      (case (elt (string-downcase fill-random-data) 0)
	((#\y #\t #\1) (setf fill-random-data t))
	((#\n #\f #\0) (setf fill-random-data nil))
	(t (error "Answer not understood")))

      (when fill-random-data
	(format t "Adding demo data...~%")
	(loop 
	  for x in '(
		     ("New York" "city" "largest city of the USA")
		     ("Washington" "city" "capital of the USA")
		     ("Paris" "city" "capital of France")
		     ("USA" "country" "")
		     ("France" "country" "")
		     ("UK" "country" "contains Greenwich")
		     )
	  do
	  (with-db
	    (clsql:insert-records 
	      :into table-name
	      :attributes (list name-column attribute-1-column attribute-2-column)
	      :values x
	      )))
	(format t "Done.~%"))

      (loop 
	for s in '("plugins" "queries" "results")
	do
	(ensure-directories-exist
	  (concatenate 'string target-directory "/" s "/")))

      (setf
	source-directory 
	(compile-time-value
	  (namestring
	    (make-pathname 
	      :directory 
	      (pathname-directory
		(or *compile-file-pathname* *load-pathname*))))))

      (loop
	for plugin in '("sql2.lisp" "sql2.peg" "generic.peg" "generic-readers.lisp" 
			"lisp-query.lisp" "encrypt-file-content.lisp" "password-store.lisp")
	do
	(with-open-file (f (concatenate 'string source-directory "/example-plugins/" plugin))
	  (with-open-file (g (concatenate 'string target-directory "/plugins/" plugin)
			     :direction :output :if-exists :supersede :if-does-not-exist :create)
	    (princ (read-stream-contents f) g)
	    )
	  )
	)

      (with-open-file (g (concatenate 'string target-directory "/queries/" "db-demo-connection.sql2")
			 :direction :output :if-exists :supersede :if-does-not-exist :create)
	(format g "set db-server=~s~%" server-name)
	(format g "set db-name=~s~%" database-name)
	(format g "set db-type=~s~%" (string-downcase (symbol-name database-type)))
	(format g "set db-user=~s~%" database-user)
	(format g "set db-password=~s~%" database-password)
	)
      (with-open-file (g (concatenate 'string target-directory "/queries/" "db-demo-query.sql2")
			 :direction :output :if-exists :supersede :if-does-not-exist :create)
	(format 
	  g 
	  "
	  mkdir \"test-table\" do
	  grouped-for row in \"select ~a, ~a, ~a from ~a limit 1000\" do
	  with-file \"~a\" do
	  on-read $row[0]
	  done
	  with-file \"~a\" do
	  on-read $row[1]
	  done
	  with-file \"~a\" do
	  on-read $row[2]
	  done
	  done
	  done
	  "
	  (clsql:sql name-column)
	  (clsql:sql attribute-1-column)
	  (clsql:sql attribute-2-column)
	  (clsql:sql table-name)
	  (clsql:sql name-column)
	  (clsql:sql attribute-1-column)
	  (clsql:sql attribute-2-column)
	  )
	))

    (read-variable
      "Please specify whether to skip mounting QueryFS" skip-launch "no")
    (case (elt (string-downcase skip-launch) 0)
      ((#\y #\t #\1) (setf skip-launch t))
      ((#\n #\f #\0) (setf skip-launch nil))
      (t (error "Answer not understood")))

    (unless skip-launch (run-fs :target target-directory))
    ))

(export 'queryfs-sql-demo)
