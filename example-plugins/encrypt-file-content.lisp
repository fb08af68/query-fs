(in-package :query-fs)

(load-plugin "generic-readers.lisp")
(load-plugin "password-store.lisp")

(defun mkfile-add-encryption (password file-expression value-to-string)
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
    (old-content (and is-file (getf (cdr expanded-expression) :contents)))
    (new-content (and old-content 
                      `(lambda 
                        ()
                        (let ((c (funcall ,old-content))) 
                             (when 
                              c 
                              (cl-fuse::string-to-octets 
                               (unencrypt-aes 
                                ,(funcall value-to-string 'c)
                                ,(funcall value-to-string password))
                               :direct))))))
    (old-writer (and is-file (getf (cdr expanded-expression) :writer)))
    (new-writer (and old-writer 
                     `(lambda (x) 
                              (funcall 
                               ,old-writer 
                               (encrypt-aes 
                                ,(funcall value-to-string 'x)
                                ,(funcall value-to-string password))))))
    )
   (when is-file 
         (setf (getf (cdr expanded-expression) :contents) new-content)
         (when old-writer
               (setf (getf (cdr expanded-expression) :writer) new-writer)
               )
         (cl-fuse::fuse-complain "Enc-file: processed to ~s~%" expanded-expression)
         expanded-expression
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
