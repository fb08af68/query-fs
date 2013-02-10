(in-package :query-fs)

(defmacro def-reader (f args &rest body)
  `(defun ,(intern (concatenate 'string (string f) "-READER")) 
          (read-stream ,@args &rest extra-args)
          (declare 
           (ignorable extra-args)
           )
          ,@body))

(def-reader lisp-escape ()
  (eval `(block nil ,(read read-stream) t)))

(defun parse-indexed-code (read-code)
  (cond
   ((or
     (stringp read-code)
     (symbolp read-code)
     )
    read-code)
   ((and
     (listp read-code)
     (stringp (first read-code))
     )
    `(format nil ,@(mapcar 'parse-indexed-code read-code)))
   ((and
     (listp read-code)
     (= (length read-code) 2)
     (symbolp (first read-code))
     (integerp (second read-code))
     )
    `(elt ,@read-code))
   ((and
     (listp read-code)
     (symbolp (first read-code))
     (fboundp (first read-code))
     )
    read-code)
   ((and
     (listp read-code)
     (= (length read-code) 2)
     (listp (first read-code))
     (integerp (second read-code))
     )
    `(elt 
      ,(parse-indexed-code 
        (first read-code))
      ,(second read-code)))
   (t nil)))

(def-reader indexed ()
  (let ((read-code (read read-stream)))
       (parse-indexed-code read-code)))

(def-reader file ()
  `(mk-file ,(indexed-reader read-stream)
            ,(indexed-reader read-stream)))

(def-reader symlink ()
    `(mk-symlink ,(indexed-reader read-stream)
                 ,(indexed-reader read-stream)))

(def-reader sink ()
  `(mk-file ,(indexed-reader read-stream)
            ,(indexed-reader read-stream)
            ,(read read-stream)))

(def-reader dir ()
  `(mk-dir ,(indexed-reader read-stream)
           :just 
           ,@(collect-entries read-stream *query-parser*)))

(def-reader preserve-vars (vars)
  `(progv ',vars `,,(loop for v in vars collect '(unquote var))
          ,(collect-entries read-stream *query-parser*)))

(def-reader direct-list ()
  (let ((var (read read-stream)))
       `(mk-pair-generator
         ,var
         ,@(progn (read read-stream) nil)
         ',(mapcar (lambda (x) (list x x)) 
                   (read read-stream))
         (mk-dir 
          (car ,var) :eval
          (let ((,var (second ,var)))
          (list ,@(collect-entries read-stream *query-parser*)))))))

(def-reader proxy ()
  (let ((name (indexed-reader read-stream))
        (val (indexed-reader read-stream)))
       `(mk-file ,name ,val (data (setf ,val data)))
   ))
(def-reader setter ()
  (let ((name (indexed-reader read-stream))
        (val (indexed-reader read-stream)))
       `(mk-file ,name "" (data (setf ,val data)))
   ))
