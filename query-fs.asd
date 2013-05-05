; Query-FS
; Based on cl-fuse FUSE bindings for Common Lisp
; Distributed as free software under Lisp Library General Public License
; Distributed AS IS with NO WARRANTY

(asdf:defsystem :query-fs
  :name "query-fs"
  :author "Michael Raskin <fb08af68@rambler.ru>"
  :maintainer "Michael Raskin <fb08af68@rambler.ru>"
  :license "LLGPL"
  :description "High-level virtual FS using CL-Fuse-Meta-FS to represent results of queries"
  :depends-on (
               :cl-fuse
               :cl-fuse-meta-fs
               :iterate
               :cl-ppcre
               :bordeaux-threads
               :trivial-backtrace
	       :command-line-arguments
               )
  :serial t
  :components (
	       (:file "package")
               (:file "query-fs")
               ))
