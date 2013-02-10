(in-package :query-fs)
(cffi:define-foreign-library libcrypt
  (t (:default "libcrypt")))
(cffi:use-foreign-library libcrypt)

(load-plugin "encrypt-file-content.lisp")
(require :clsql)
(require :esrap-peg)
(import 'esrap-peg:def-peg-fun)
(import 'esrap-peg:ast-eval)
(import 'esrap-peg:def-peg-matchers)

(defpackage :query-fs-sql2 (:use))

(in-package :query-fs-sql2)
(esrap-peg:peg-compile 
  (esrap-peg:parse-peg-file 
    (common-lisp:merge-pathnames 
      "generic.peg" 
      (common-lisp:or 
	common-lisp:*load-truename* 
	common-lisp:*compile-file-truename* 
	common-lisp:*default-pathname-defaults*))))
(esrap-peg:peg-compile 
  (esrap-peg:parse-peg-file 
    (common-lisp:merge-pathnames 
      "sql2.peg" 
      (common-lisp:or 
	common-lisp:*load-truename* 
	common-lisp:*compile-file-truename* 
	common-lisp:*default-pathname-defaults*))))
(common-lisp:in-package :query-fs)

(defvar *db-connection-spec* (list "" "" "" ""))
(defvar *db-type* :postgresql)

(defmacro variable-post-process (x) 
  `(cl-fuse::recast-string ,x cl-fuse-meta-fs::*meta-fs-name-encoding* :direct))

(setf (gethash "sql2" *query-loader-types*) :whole-file)
(setf (gethash "sql2" *query-loaders*) 'sql2-parse-text)

(defun read-stream-contents (f)
  (let* ((out-data 
	   (make-array 
	     (file-length f) 
	     :element-type 'character
	     :initial-element #\Space)))
    (read-sequence out-data f)
    out-data
    ))
(defun read-file-contents (fn)
  (with-open-file 
    (f fn)
    (read-stream-contents f)
    ))

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

(defun sql-real-value (x &key 
  (encoding cl-fuse-meta-fs::*meta-fs-name-encoding*))
  (cond
   ((stringp x) x)
   ((arrayp x) 
    (cl-fuse::octets-to-string x encoding))
   ((integerp x) (format nil "~a" x))
   (t x)))

(defun to-sql (x)
  (let*
    ((res
       (clsql:sql
	 (sql-real-value x :encoding :direct)
	 )))
    res
    ))

(defun sql-setter-translation (x)
  (cond
    ((equal (symbol-name x) "DB-SERVER") '(first *db-connection-spec*))
    ((equal (symbol-name x) "DB-NAME") '(second *db-connection-spec*))
    ((equal (symbol-name x) "DB-USER") '(third *db-connection-spec*))
    ((equal (symbol-name x) "DB-PASSWORD") '(fourth *db-connection-spec*))
    ((equal (symbol-name x) "DB-TYPE") '*db-type*)
    (t x)
    ))
(defun sql-setter-value-translation (x v)
  (cond
    ((equal (symbol-name x) "DB-TYPE")
     (intern (string-upcase (eval v)) :keyword))
    ((equal (symbol-name x) "DB-PASSWORD")
     (cl-ppcre:regex-replace-all 
       #.(concatenate 'string '(#\Newline) ".*") 
       (eval v) ""))
    (t v)
    ))

(defmacro mk-file-key
  (name &key (on-read "") (on-write nil) (on-remove nil))
  `(mk-file ,name ,on-read ,on-write ,on-remove))

(defun simplify-concatenation (l)
  (iter
    (with res := nil)
    (for x in l)
    (if
      (and (stringp x) 
	   (stringp (car res)))
      (push (concatenate 'string (pop res) x) res)
      (push x res)
      )
    (finally 
      (return (reverse res)))
    ))

(defun sql-escaped (l)
  (append
    (list 'concatenate ''string)
    (mapcar 
      (lambda (s)
	(if (stringp s) s
	  `(to-sql ,s)))
      (cddr l))))

(defun debugged (x)
  (format t "~s~%" x) x)

(defparameter *sql2-Evaluator* 'eval)

(def-peg-matchers
  (
   (identifierstart (_ x))
   (identifiercont
     ((query-fs-sql2::identifierstart ?x) ?x)
     (_ x))
   (identifier
     (_ (intern 
	  (string-upcase 
	    (apply 
	      'concatenate 'string 
	      (mapcar #'! (cons (n1 x) (n2 x))))))))
   (integer (_ (parse-integer (s+ x))))
   (indexedidentifier
     ((?var ?indexes)
      (iter
	(with main-var := (! ?var))
	(for index in (mapcar #'! (mapcar #'n2 ?indexes)))
	(for res initially main-var then (list 'elt res index))
	(finally (return res))
	)))
   (value
     ((query-fs-sql2::integer . _) (! x))
     ((query-fs-sql2::string . _) (! x))
     (("$" ?x) (! ?x))
     )
   (innervalue
     ((query-fs-sql2::integer . _) (! x))
     ((query-fs-sql2::string . _) (! x))
     (("$" ?x) (! ?x))
     ((() ?x) (! ?x))
     )
   (string
     ((_ ?x _) 
      `(concatenate 
	 'string ,@(simplify-concatenation (mapcar #'! ?x)))))
   (stringentry
     (("\\" (_ ?x)) ?x)
     (("\\u" . ?x) 
      (string (code-char (parse-integer (apply #'s+ ?x) :radix 16))))
     (("\\U" . ?x) 
      (string (code-char (parse-integer (apply #'s+ ?x) :radix 16))))
     ((query-fs-sql2::VariableExpansion . _) `(variable-post-process ,(! x)))
     ((() ?x) ?x)
     )
   (variableexpansion
     ((_ ?x _) `(let ((x ,(! ?x))) 
		  (sql-real-value x))))
   (statementbody (_ (! x)))

   (template
     ((_ _ ?name _ _ _ ?args _ _ ?body _)
      (let*
	(
	 (args (m! #'! (m! #'n1 ?args)))
	 (code `(progn (defun ,(! ?name) ,args ,(! ?body)) nil))
	 (final-code `(unless (fboundp ',(! ?name)) ,code))
	 )
	(funcall *sql2-Evaluator* code)
	final-code)))
   (CallTemplate 
     ((_ _ ?name _ _ _ ?args _) 
      `(let*
	 (
	  (args 
	    (list ,@(mapcar 
		      (lambda (x) `(progn ,x))
		      (mapcar #'! (mapcar #'n1 ?args)))))
	  (func ',(! ?name))
	  (res (apply func args))
	  )
	 res
	 )))
   (Symlink 
     ((_ _ ?name _ ?target ?remover) 
      `(mk-symlink 
	 (sql-real-value ,(! ?name) :encoding :full-range)
	 (sql-real-value ,(! ?target) :encoding :full-range)
	 ,(when ?remover 
	    (! (n4 ?remover))))))
   (WithFile ((_ _ ?name _ ?body) 
	      `(mk-file-key ,(! ?name) ,@(l+ (! ?body)))))
   (FileBlock ((_ _ ?x _) (m! #'! (m! #'n2 ?x))))
   (FileStatement ((_ ?x _) (! ?x)))
   (FileStatementBody (_ (! x)))
   (OnRead ((_ _ ?x) `(:on-read ,(! ?x))))
   (OnWrite 
     ((_ _ ?var _ ?body) 
      `(:on-write 
	 (,(! ?var) 
	   ,(! ?body)))))
   (OnNonEmptyWrite 
     ((_ _ ?var _ ?body) 
      `(:on-write 
	 (,(! ?var) 
	   (progn
	     (when
	       (> (length ,(! ?var)) 0)
	       ,(! ?body))
	     (format *error-output* "write: ~s~%" ,(! ?var))
	     t)))))
   (OnRemove
     ((_ _ ?x) 
      `(:on-remove ,(! ?x))))
   (OnRemoveDir 
     ((_ _ ?x) `(mk-dir-remover 
		  ,(! ?x))))
   (OnSymlink
     ((_ _ ?name _ ?content _ ?x)
      (let*
	(
	 (var (! ?name))
	 (cont (! ?content))
	 )
	`(mk-symlinker 
	   ,var ,cont
	   (let 
	     (
	      )
	     ,(! ?x)
	     t
	     )))))
   (IsolateDB
     ((_ _ ?body) 
      (let*
	(
	 (local-spec nil)
	 (local-type nil)
	 (*db-connection-spec* (subseq *db-connection-spec* 0))
	 (*db-type* *db-type*)
	 (body-eval (! ?body))
	 )
	(setf local-spec *db-connection-spec*)
	(setf local-type *db-type*)
	`(let
	   (
	    (*db-connection-spec* ',local-spec)
	    (*db-type* ',local-type)
	    )
	   (macrolet
	     (
	      (with-db 
		(&rest body)
		`(let*
		   (
		    (*db-connection-spec* ',',local-spec)
		    (*db-type* ',',local-type)
		    )
		   ,(macroexpand `(with-db ,@body))
		  )
		)
	      )
	     ,@ body-eval
	     )
	   )
	)
      )
     )
   (SQLText
     ((?data ?enc) 
      (list (! ?data) 
	    (if 
	      ?enc 
	      (intern (string-upcase (eval (! (n4 ?enc)))) :keyword) 
	      :direct))))
   (SQLCommand
     (?x 
       (let*
	 (
	  (code (! ?x))
	  )
	 `(macrolet
	    (
	     (variable-post-process 
	       (x)
	       `(cl-fuse::recast-string 
		  ,x cl-fuse-meta-fs::*meta-fs-name-encoding*
		  ,,(second code))
	       )
	     )
	    (with-db (clsql:execute-command ,(sql-escaped (first code))) t)))))
   (SQLQuery
     (?x 
       (let*
	 (
	  (code (! ?x))
	  )
	 `(mapcar 
	    (lambda (e)
	      (list (fmt "~a" 
			 (sql-real-value (first e)))
		    e))
	    (macrolet
	      (
	       (variable-post-process 
		 (x)
		 `(cl-fuse::recast-string 
		    ,x cl-fuse-meta-fs::*meta-fs-name-encoding*
		    ,,(second code))
		 )
	       )
	      (let*
		(
		 (q ,(sql-escaped (first code)))
		 (d (with-db (clsql:query q)))
		 (dd (decode-sql-strings d :encoding ,(second code)))
		 )
		;(cl-fuse::fuse-complain "Query: ~s~%Data: ~s ~%:: ~s" q d dd)
		dd)))
	 )))
   )
  :abbreviations
  (
   (! (x) (ast-eval x))
   (n1 (x) (first x)) (n2 (x) (second x)) (n3 (x) (third x))
   (n4 (x) (fourth x)) (n5 (x) (fifth x)) (n6 (x) (sixth x))
   (n7 (x) (seventh x)) (n8 (x) (eighth x)) (n9 (x) (ninth x))
   (s+ (x) (apply 'concatenate 'string x))
   (l+ (x) (apply 'append x))
   (m! (f x) (mapcar f x))
   )
  :package :query-fs-sql2
  :arg x
  )

(def-peg-fun
  query-fs-sql2::SetParam (x)
  (funcall
    *sql2-Evaluator* `(setf 
		   ,(sql-setter-translation (ast-eval (third x))) 
		   ,(sql-setter-value-translation
		      (ast-eval (third x))
		      (ast-eval (Seventh x)))))
  `(progn
     '(setf 
	,(sql-setter-translation (ast-eval (third x))) 
	,(sql-setter-value-translation
	   (ast-eval (third x))
	   (ast-eval (Seventh x))))
     nil))

(def-peg-fun
  query-fs-sql2::ReadParam (x)
  (funcall
    *sql2-Evaluator* 
    `(setf ,(sql-setter-translation (ast-eval (third x))) 
	   ,(sql-setter-value-translation 
	      (ast-eval (third x))
	      (read-file-contents (eval (ast-eval (Seventh x)))))
	   ))
  `(progn
     '(setf ,(sql-setter-translation (ast-eval (third x))) 
	    ,(sql-setter-value-translation 
	       (ast-eval (third x))
	       (read-file-contents (eval (ast-eval (Seventh x))))))
     nil))

(def-peg-fun
  query-fs-sql2::declaretransient (x)
  (funcall
    *sql2-Evaluator* `(defvar ,(ast-eval (third x)) ,(ast-eval (fifth x))))
  `(progn '(defvar ,(ast-eval (third x)) ,(ast-eval (fifth x))) nil))

(def-peg-fun
  query-fs-sql2::declaretransientset (x)
  (funcall
    *sql2-Evaluator* 
    `(progn
       (defvar ,(ast-eval (third x)))
       (setf ,(ast-eval (third x)) 
	     ,(ast-eval (fifth x)))))
  `(progn '(defvar ,(ast-eval (third x)))
	  '(setf ,(ast-eval (third x)) 
		 ,(ast-eval (fifth x)))
	  nil))

(def-peg-fun
  query-fs-sql2::proxy (x)
  (let*
    (
     (content (Ast-eval (fifth x)))
     )
    `(mk-file-key
       ,(ast-eval (third x))
       :on-read ,content
       :on-write (data (setf ,content data) 0)
       )))

(def-peg-fun
  query-fs-sql2::setter (x)
  (let*
    (
     (content (Ast-eval (fifth x)))
     )
    `(mk-file-key
       ,(ast-eval (third x))
       :on-read ""
       :on-write (data (setf ,content data) 0)
       )))

(def-peg-fun
  query-fs-sql2::block (x)
  (mapcar 'ast-eval
	  (mapcar 'second
		  (third x))))

(def-peg-fun
  query-fs-sql2::oncreatedir (x)
  (let*
    (
     (var (ast-eval (third x)))
     )
    `(mk-creator
       ,var
       nil
       (let
	 ()
	 ,(ast-eval (fifth x))))))

(def-peg-fun
  query-fs-sql2::oncreatefile (x)
  (let*
    (
     (var (ast-eval (third x)))
     )
    `(mk-creator
       ,var
       (let
	 ()
	 ,(ast-eval (fifth x)))
       nil
       )))

(def-peg-fun
  query-fs-sql2::mkdir (x)
  `(mk-dir
     ,(ast-eval (third x))
     :just 
     ,@(ast-eval (fifth x))))

(def-peg-fun
  query-fs-sql2::encryptby (x)
  (mkfile-add-encryption
    (ast-eval (third x))
    (ast-eval (seventh x))
    (lambda (x) `(sql-real-value ,x :encoding :direct))))

(def-peg-fun
  query-fs-sql2::sqlfor (x)
  (let*
    (
     (var (ast-eval (third x)))
     )
  `(mk-pair-generator 
     ,var
     (let*
       ()
       ,(ast-eval (seventh x))
       )
       (progn
	 ;(cl-fuse::fuse-complain "Name: ~s~%" ,var)
	 (let* ((name 
		  (cl-fuse::string-to-octets
		    (first ,var)
		    cl-fuse-meta-fs::*meta-fs-name-encoding*))
		(,var (second ,var)))
	   (declare (ignorable ,var))
	   ,(ast-eval (ninth x))
	   )))))

(def-peg-fun
  query-fs-sql2::sqlgroupedfor (x)
  (let*
    (
     (var (ast-eval (third x)))
     )
    `(mk-pair-generator 
       ,var
       ,(ast-eval (seventh x))
       (mk-dir (car ,var) :eval
	       (let ((,var (cadr ,var)))
		 (list
		   ,@(ast-eval (ninth x))
		   )
		 )))))

(def-peg-fun 
  query-fs-sql2::file (x)
  (mapcar 'ast-eval
	  (mapcar 'first
		  (second x))))

(defun sql2-parse-text (f)
  (ast-eval
    (esrap:parse
      'query-fs-sql2::file
      (read-stream-contents f))))
