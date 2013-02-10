sink "reload" "" (data (progn data (reload-files) t))
sink "load-new" "" (data (progn data (load-new-files) t))
sink "reload-plugins" "" (data (progn data 
				      (setf query-fs::*plugins-loaded*
					    (make-hash-table :test 'equal)) 
				      (cl-fuse::fuse-complain 
					"Reload-plugins: ~s~%" 
					(query-fs::reload-plugins)) t))
init (progn 
  (defvar *execution-result-storage*)
  (setf *execution-result-storage* nil))
sink "exec" (fmt "~s~%" *execution-result-storage*) 
(data 
  (let* ((str (ignore-errors (cl-fuse::octets-to-string data :direct)))
         (code (ignore-errors (with-input-from-string (s str) (read s))))
         (val (ignore-errors (eval code)))
         )
        (setf *execution-result-storage* val)))
sink "umount" "" (data (progn data (
  #+sbcl sb-ext:run-program 
  #+ccl ccl:run-program
  #+ecl ext:run-program
  "fusermount" 
  `("-u" "/home/raskin/mnt/query-fs/results/")
  #+sbcl :search #+sbcl t :wait nil)))
sink "forget-query" "" 
  (data (progn
         (when (> (length data) 0)
               (when (equal (elt data (- (length data) 1)) 10) 
                     (setf data (subseq data 0 (- (length data) 1))))
               (remove-query 
                (cl-fuse::octets-to-string data :direct)))
         t))
sink "forget-plugin" "" 
  (data (progn
         (when (> (length data) 0)
               (when (equal (elt data (- (length data) 1)) 10) 
                     (setf data (subseq data 0 (- (length data) 1))))
               (remhash 
                (cl-fuse::octets-to-string data :direct)
		query-fs::*plugins-loaded*))
         t))
file "last-error" cl-fuse::*last-wrapper-error*
symlink "queries" (concatenate 'string *target* "/queries")
symlink "plugins" (concatenate 'string *target* "/plugins")
symlink "backing-store" (concatenate 'string *target* "/backing-store")
file "threads" (format nil "~{~a~%~}" (mapcar 'bordeaux-threads:thread-name (bordeaux-threads:all-threads)))
