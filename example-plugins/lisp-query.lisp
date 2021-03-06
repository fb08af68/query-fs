(in-package :query-fs)

(load-plugin "generic-readers.lisp")

(def-reader lisp-with ()
  (let ((var (read read-stream)))
       `(mk-pair-generator
         ,var
         ,@(progn (read read-stream) nil)
         ,(read read-stream)
         (mk-dir
          (car ,var) :eval
          (let ((,var (second ,var)))
               (list ,@(collect-entries read-stream *query-parser*)))))))

(def-reader lisp-for ()
  (let ((var (read read-stream)))
       `(mk-pair-generator 
         ,var
         ,@(progn (read read-stream) nil)
         ,(read read-stream)
         (let ((name (first ,var))
               (,var (second ,var)))
              (declare (ignorable ,var))
              ,(funcall *query-parser* read-stream)))))

(def-reader lisp-when ()
  (let ((condition (read read-stream))
	(var (gensym))
	(entries (collect-entries read-stream *query-parser*)))
    `(mk-pair-generator
       ,var
       (progn
	 (when ,condition
	   (loop for ,var in (list ,@entries)
	         collect (list (getf ,var :name) ,var)))))))

(def-reader lisp-if-then-else ()
  (let ((condition (read read-stream))
        (then (read read-stream))
        (entries (collect-entries read-stream *query-parser*))
        (entries-else (collect-entries read-stream *query-parser*))
        )
    then
    `(if ,condition
       (progn ,@entries)
       (progn ,@entries-else)
       )))

(def-reader lisp-template ()
  (let*
    ((name (read read-stream))
     (args (read read-stream))
     (entries (collect-entries read-stream *query-parser*))
     (code `(defun ,name (,@args) ,@entries))
     )
    (progn
      (cl-fuse::fuse-complain "Defining template: ~s~%" code)
      (eval code) t)))

(def-reader lisp-call ()
  `(,(read read-stream) ,@(read read-stream)))

(def-linear-query-parser "lisp"
  ((init) lisp-escape-reader)
  ((literal) indexed-reader)
  ((file) file-reader)
  ((sink) sink-reader)
  ((symlink) symlink-reader)
  ((dir) dir-reader)
  ((with) lisp-with-reader)
  ((for) lisp-for-reader)
  ((when) lisp-when-reader)
  ((if) lisp-if-then-else-reader)
  ((template) lisp-template-reader)
  ((call) lisp-call-reader)
  ((end else nil :eof) nil)
  (t nil))
