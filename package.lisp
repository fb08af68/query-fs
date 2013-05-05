; Query-FS
; Based on cl-fuse FUSE bindings for Common Lisp
; Distributed as free software under Lisp Library General Public License
; Distributed AS IS with NO WARRANTY

(defpackage :query-fs
  (:use 
    :cl-fuse-meta-fs :iterate :common-lisp
    :command-line-arguments
    )
  (:export
   #:*query-parser*
   #:*target*
   #:*query-path*
   #:*plugin-path*
   #:*result-path*
   #:backtrace-error
   #:update-description
   #:load-plugin
   #:load-query
   #:collect-entries
   #:def-query-parser
   #:def-linear-query-parser
   #:reload-files
   #:run-fs
   #:run-fs-with-cmdline-args
   #:fmt

   #:load-demo
   )
  )


