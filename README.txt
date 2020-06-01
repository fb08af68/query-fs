Query-FS is a virtual filesystem implemented using FUSE in Common Lisp.
Its current main usecase is providing a POSIX FS API to access and modify some
data in SQL databases.

One can write:

mkdir "all" do
  for x in "select name, content from test_table"
    with-file $name do
      on-read $x[1]
      on-write data "update test_table set content = ${data} where name = ${name}"
      on-remove "delete from test_table where name = ${name}"
    done
  on-create-file name "insert into test_table (name) values (${name})"
done

and browse and modify a DB table as a directory.

One can also write some queries in Common Lisp:

(mk-pair-generator x
  (let ((xn (ignore-errors (parse-integer (first x)))))
    (if xn `((,(first x) ,(1+ xn)))
      (loop for k from 1 to 10 collect `(,(format nil "~a" k) ,(1+ k)))))
  (mk-file (first x) (format nil "~a" (second x))))

Installation and use:

$ cd ~/quicklisp/local-projects
$ git clone https://gitlab.common-lisp.net/cl-fuse/query-fs
$ mkdir -p query-fs-test/queries
$ $EDITOR query-fs-test/queries/…
* (ql:quickload :query-fs)
* (query-fs:run-fs :target "query-fs-test")

See example queries in example-queries and clsql-queries
