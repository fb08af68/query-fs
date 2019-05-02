(in-package :query-fs)

(load-plugin "generic-readers.lisp")
(load-plugin "password-store.lisp")

(defun mkfile-add-encryption (password file-expression value-to-string &key (old t))
  (let*
    (
     (macroexpanded-expression (macroexpand file-expression))
     (expanded-expression (mapcar (lambda 
                                    (x) 
                                    (cond
                                      ((and
                                         (listp x)
                                         (= (length x) 2)
                                         (eq (car x) 'quote)
                                         (symbolp (cadr x))
                                         (equal 
                                           (package-name (symbol-package (cadr x)))
                                           "KEYWORD"
                                           )
                                         ) (cadr x))
                                      (t x)))
                                  macroexpanded-expression))
     (backq-form (first '`(,(+))))
     (is-file (and 
                (find (first expanded-expression) 
                      (list backq-form 'quote 'list))
                (eq (getf (cdr expanded-expression) :type) :file)
                ))
     (is-symlink (and 
                   (find (first expanded-expression) 
                         (list backq-form 'quote 'list))
                   (eq (getf (cdr expanded-expression) :type) :symlink)
                   ))
     (old-content (and (or is-file is-symlink)
                       (getf (cdr expanded-expression) :contents)))
     (new-content (and old-content 
                       (progn
                         (cl-fuse::fuse-complain
                           "Enc: old-content: ~s~%" old-content)
                         t)
                       `(lambda 
                          ()
                          (let* ((c (funcall ,old-content))
                                 (c-str (and c ,(funcall value-to-string 'c)))
                                 (p-str ,(funcall value-to-string password))
                                 (ue (and c (if ,old (unencrypt-aes c-str p-str)
                                              (unencrypt-aes-new c-str p-str))))
                                 (ueo (and 
                                        c 
                                        (cl-fuse::string-to-octets 
                                          ue :direct)
                                        )))
                            ,(cond 
                               (is-symlink 
                                 '(cl-fuse::recast-string 
                                    ue :direct 
                                    cl-fuse-meta-fs::*meta-fs-name-encoding*))
                               (t 'ueo))))))
     (old-writer (and is-file (getf (cdr expanded-expression) :writer)))
     (new-writer (and old-writer 
                      `(lambda (x) 
                         (funcall 
                           ,old-writer 
                           (if ,old
                             (encrypt-aes 
                               ,(funcall value-to-string 'x)
                               ,(funcall value-to-string password))
                             (encrypt-aes-new
                               ,(funcall value-to-string 'x)
                               ,(funcall value-to-string password)))))))
     )
    (or
      (when is-file 
        (setf (getf (cdr expanded-expression) :contents) new-content)
        (when old-writer
          (setf (getf (cdr expanded-expression) :writer) new-writer)
          )
        (cl-fuse::fuse-complain "Enc-file: processed to ~s~%" expanded-expression)
        expanded-expression
        )
      (when is-symlink
        (setf (getf (cdr expanded-expression) :contents) new-content)
        (cl-fuse::fuse-complain "Enc-symlink: processed to ~s~%" expanded-expression)
        expanded-expression
        )
      )
    ))

(def-reader encrypted-file (value-to-string)
  (cl-fuse::fuse-complain "Starting encrypted-file processing~%")
  (let*
   (
    (dummy (read read-stream))
    (password (read read-stream))
    (file-expression (funcall *query-parser* read-stream))
    )
   (declare (ignore dummy))
   (mkfile-add-encryption password file-expression value-to-string)
   ))
