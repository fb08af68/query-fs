To run a minimal test, create /tmp/test-query-fs/ and copy example-queries as
queries there, and example-plugins as plugins. Then load query-fs and run

(query-fs:run-fs :target "/tmp/test-query-fs/")

in your Common Lisp REPL. Example command for SBCL and Quicklisp:

sbcl --load setup.lisp --eval '(quicklisp:quickload :query-fs)' --eval '(query-fs:run-fs :target "/tmp/test-query-fs/")' --eval '(quit)'

