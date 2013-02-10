(in-package :query-fs)

(defun process-sexp-subtree (x)
  (cond
   ((null x) nil)
   ((not (listp x)) nil)
   ((listp (second x))
    `(mk-dir ,(first x) :just
             ,@(mapcar 'process-sexp-subtree (cdr x))))
   ((stringp (second x))
    `(mk-file ,(first x) ,(second x)))
   (t nil)))

(def-query-parser "sexp"
  (ignore-errors 
    (process-sexp-subtree 
     (ignore-errors (read read-stream)))))
