init (require :sb-posix)
init (clsql-sys:locally-enable-sql-reader-syntax)
init (defvar *last-indexed-file* nil)
init (setf *last-indexed-file* "")
sink "index-file" *last-indexed-file*
(data 
  #+sbcl (let* ((n (- (length data) 1))
         (data 
          (if (and
               (>= n 0)
               (equal (elt data n) 10)
               )
              (subseq data 0 n)
              data))
         (str (cl-fuse::octets-to-string data :full-range))
         (st (ignore-errors (sb-posix:stat str)))
         (pn (ignore-errors (parse-namestring str)))
         (dirp (ignore-errors (sb-posix:s-isdir (sb-posix:stat-mode st))))
         )
        (setf *last-indexed-file* str)
        (cl-fuse:fuse-complain "~s~%" (list data str st pn dirp))
        (with-db
         (ignore-errors
          (clsql:delete-records :from [indexed-files]
                                :where [= [path] str])
          (clsql:insert-records :into [indexed-files]
                                :av-pairs `(
                                            (path ,str)
                                            (name ,(or
                                                    (pathname-name pn)
                                                    (car 
                                                     (last 
                                                      (pathname-directory 
                                                       pn)))
                                                    ))
                                            (type ,(pathname-type pn))
                                            (size ,(sb-posix:stat-size st))
                                            (is_directory ,(if dirp "t" "f"))
                                            ))))
        t
  ))
sink "unindex-file" *last-indexed-file*
(data 
  #+sbcl (let* ((n (- (length data) 1))
         (data 
          (if (equal (elt data n) 10)
              (subseq data 0 n)
              data))
         (str (sb-ext:octets-to-string data :external-format :utf-8))
         )
        (setf *last-indexed-file* str)
        (with-db
         (ignore-errors
          (clsql:delete-records :from [indexed-files]
                                :where [= [path] str])
          ))
        t
  ))
init (clsql-sys:restore-sql-reader-syntax-state)

