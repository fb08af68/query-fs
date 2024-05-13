; Query-FS
; Based on cl-fuse FUSE bindings for Common Lisp
; Distributed as free software under Lisp Library General Public License
; Distributed AS IS with NO WARRANTY

(in-package :query-fs)

(defvar *description* nil)

(cffi:defcfun "getenv" :string (var-name :string))
(cffi:defcfun "setenv" :void (var-name :string) (val :string) (overwrite :int))
(defvar *HOME*)
(setf *HOME* (getenv "HOME"))

(defvar *target* 
  (concatenate 'string
               *HOME*
               "/mnt/query-fs"
               ))
(defvar *query-path*
  (concatenate 'string *target* "/queries"))
(defvar *plugin-path*
  (concatenate 'string *target* "/plugins"))
(defvar *result-path*
  (concatenate 'string *target* "/results"))

(defvar *query-loaders* (make-hash-table :test 'equalp))
(defvar *query-loader-types* (make-hash-table :test 'equalp))

(defvar *plugins-loaded* (make-hash-table :test 'equal))
(defvar *queries-loaded* (make-hash-table :test 'equal))

(defvar *query-parser*)
(setf *query-parser* nil)

(defun prepare-description (desc)
  (eval
   `(append 
     (mk-dir ,(cl-ppcre:regex-replace ".*/" *result-path* "") :eval ',desc)
     `(:root t)
     )))

(defun update-description ()
  (setf cl-fuse-meta-fs::*description* (prepare-description *description*))
  )

(defun backtrace-error (e &optional extra-text)
  (format *error-output* "Error ~s: ~s~%" (or extra-text "") e)
  (format *error-output* "Error readably ~a: ~a~%" (or extra-text "") e)
  (trivial-backtrace:print-backtrace-to-stream *error-output*)
  )

(defun load-plugin (file-path)
  (let* (
         (real-file-path 
          (if (probe-file file-path)
              file-path
              (concatenate 
		'string *plugin-path* "/" file-path)
           ))
         (pn (pathname real-file-path))
         (fn (pathname-name pn))
         )
        (block nil
               (handler-bind 
                ((error
                  (lambda (e) 
                          (backtrace-error 
                           e 
                           (format nil "in plugin ~s" 
                                   real-file-path))
                          (return nil))))
                (unless 
                 (gethash fn *plugins-loaded*)
                 (load real-file-path)
                 (setf (gethash fn *plugins-loaded*) t)
                 )
                ))
        ))

(defun reload-plugins ()
  (mapcar 'load-plugin
          (directory
           (concatenate 'string *plugin-path* "/*.lisp"))))

(defun collect-entries (file func)
  (iter (for entry := (funcall func file))
        (while entry)
        (unless (eq entry t) (collect entry))
        ))

(defun load-query (file-name)
  (let* (
	 (pn (pathname file-name))
	 (ft (pathname-type pn))
	 (fn (pathname-name pn))
	 (pkgn 
	   (concatenate 'string
			"QUERY-FS."
			(string-upcase fn)))
	 (pkg
	   (let 
	     ((pkg-old (find-package pkgn)))
	     (if pkg-old pkg-old
	       (make-package 
		 pkgn
		 :use '(:common-lisp
			 :query-fs
			 ))
	       )))
	 )
    (iter (for symb in-package :query-fs)
	  (unless 
	    (second 
	      (multiple-value-list
		(find-symbol (symbol-name symb) pkg)))
	    (import symb pkg)))
    (block 
      nil
      (progv '(*package* *query-parser*) 
	`(,pkg ,(gethash ft *query-loaders*))
	(when *query-parser*
	  (with-open-file 
	    (f file-name :external-format #-clisp :utf-8 #+clisp "utf-8")
	    (handler-bind 
	      ((error 
		 (lambda (e) 
		   (backtrace-error 
		     e 
		     (format nil "in query ~s" 
			     file-name))
		   (return nil))))
	      (unless 
		(gethash fn *queries-loaded*)
		(let*
		  ((content 
		     (case (gethash ft *query-loader-types*)
		       ((:token-by-token nil) 
			(collect-entries f *query-parser*))
		       ((:whole-file) 
			(funcall *query-parser* f))
		       ))
		   (code `(mk-dir ,fn :just
				  ,@content))
		   (query-fn (eval code))
		   )
		  (cl-fuse::fuse-complain 
		    "Content ~s~%Code ~s~%Compiled ~s~%from ~s~%type ~s~%"
		    content code query-fn file-name ft)
		  (setf (gethash fn *queries-loaded*) query-fn)
		  query-fn
		  )
		))))))))

(defun reload-queries ()
  (mapcar 'load-query
          (directory
           (concatenate 
	     'string *query-path* "/*.*"))))

(defmacro def-query-parser (query-type &rest body)
  (let* (
         (f-name (concatenate 'string "query-loader-" query-type))
         (f-sym (intern (string-upcase f-name)))
         )
        `(progn
          (defun ,f-sym (read-stream)
                 ,@body)
          (setf (gethash ,query-type *query-loaders*) ',f-sym)
          )
        ))

(defmacro def-linear-query-parser (query-type &rest body)
  `(def-query-parser 
    ,query-type
    (let ((first-query-entry (read read-stream nil :eof)))
         (case first-query-entry
               ,@(mapcar 
                  (lambda (entry) 
                          `(,(first entry) 
                             ,(if (second entry) 
                                  `(,(second entry) read-stream 
                                     ,@(cddr entry))
                                  nil
                                  )))
                  body)))))

(defun load-new-files ()
  (reload-plugins)
  (setf *description* (append *description* (reload-queries)))
  (update-description)
  )

(defun reload-files ()
  (setf *plugins-loaded* (make-hash-table :test 'equalp))
  (setf *queries-loaded* (make-hash-table :test 'equalp))
  (setf *query-loaders* (make-hash-table :test 'equalp))
  (setf *description* nil)
  (load-new-files)
  )

(defun remove-query (name)
  (setf *description* 
        (remove (gethash name *queries-loaded*) 
                *description*)
        )
  (remhash name *queries-loaded*)
  (update-description)
  (cl-fuse::fuse-complain "Name to remove: ~s~%" name)
  )

(defmacro compile-time-value (x) (eval x))

(defun run-fs 
  (&key 
    target 
    query-path plugin-path result-path
    (break-on-errors t)
    (object-cache-duration 3)
    (call-manager t)
    (thread-pool-size 8)
    (extra-fuse-args nil)
    )
  (let*
    (
     )
    (when target 
      (setf *target* target))
    (setf *query-path* (or query-path (concatenate 'string *target* "/queries")))
    (setf *plugin-path* (or plugin-path (concatenate 'string *target* "/plugins")))
    (unless (probe-file (format nil "~a/." *plugin-path*))
      (setf *plugin-path* 
            (format nil "~a/example-plugins" 
                    (cl-ppcre:regex-replace
                      "/[^/]+/?$"
                      (namestring
                        (compile-time-value
                          (or *compile-file-pathname* *load-pathname*)))
                      ""))))
    (setf *result-path* (or result-path (concatenate 'string *target* "/results")))
    (setf cl-fuse::*break-on-errors* break-on-errors)
    (setf cl-fuse-meta-fs:*object-cache-duration* 
	  object-cache-duration)
    (reload-files)
    (format t "~s~%" (list 
		       *target* *plugin-path* *query-path* *result-path*))
    (bordeaux-threads:join-thread 
      (bordeaux-threads:make-thread 
	(let
	  (
	   (result *result-path*)
	   )
	  (lambda ()
	    (handler-bind 
	      ((error
		 (lambda (e) 
		   (backtrace-error 
		     e "in filesystem"))))
	      (cl-fuse-meta-fs:run-lisp-meta-fs 
		*description*
		result
		call-manager
		thread-pool-size
		extra-fuse-args
		))
	    ))
	:name "Fuse thread"))

    #+sbcl (sb-ext:quit)
    #+ccl (ccl:quit)
    #+gcl (lisp:quit)
    #+ecl (ext:quit)
    #+clisp (ext:quit)
    )
  )

(defun run-fs-with-cmdline-args (&optional prepended-arguments)
  (let*
    (
     (spec
       `(
	 (("target" #\t) :type string :documentation ,(format nil "Specify directory containing plugin directory (plugins/), query directory (queries/), and mountpoint (results/); can be also specified as non-option argument. Default value is ~a" *target*))
	 (("plugin-path" #\p) :type string :documentation "Override directory containing plugins")
	 (("query-path" #\q) :type string :documentation "Override directory containing queries")
	 (("result-path" #\r) :type string :documentation "Override mountpoint")
	 ("call-manager" :type boolean :initial-value t :documentation "Whether to use multithreaded call manager")
	 ("thread-pool-size" :type integer :initial-value 8 :documentation "Number of threads in the pool")
	 ("object-cache-duration" :type integer :initial-value 3 :documentation "Cache retention time for objects in seconds")
	 ("break-on-errors" :type boolean :initial-value nil :documentation "Enable to break on errors")
	 (("fuse-option" #\o) :type string :list t :initial-value nil :documentation "FUSE option")
	 (("help" #\h) :type nil :documentation "Display this help")
	 )
       )
     (args (uiop:command-line-arguments))
     (opts (multiple-value-list 
	     (process-command-line-options 
	       spec (append prepended-arguments args))))
     (opts
       (append (first opts) (let ((tgt (first (second opts)))) (when tgt (list :target tgt)))))
     (fuse-opt-string (format nil "~{~a~#[~:;,~]~}" (getf opts :fuse-option)))
     )
    (if
      (getf opts :help)
      (show-option-help spec)
      (format
	t "~s~%" 
	(
	 run-fs 
	 :target (getf opts :target)
	 :query-path (getf opts :query-path)
	 :plugin-path (getf opts :plugin-path)
	 :result-path (getf opts :result-path)
	 :object-cache-duration (getf opts :object-cache-duration)
	 :call-manager (getf opts :call-manager)
	 :thread-pool-size (getf opts :thread-pool-size)
	 :break-on-errors (getf opts :break-on-errors)
	 :extra-fuse-args (when (> (length fuse-opt-string) 0) 
			    (list "-o" fuse-opt-string))
	 ))
      )
    )
  )

(defun load-demo (name) 
  (load 
    (concatenate
      'string 
      (compile-time-value
	(namestring
	  (make-pathname 
	    :directory 
	    (pathname-directory
	      (or *compile-file-pathname* *load-pathname*)))))
      name)))
