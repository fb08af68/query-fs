(defparameter *sum-numbers-range* 13)

(mk-splice
  (mk-file "README" "A POSIX interface to #'CL:+")
  (mk-pair-generator x
    (loop for k from 1 to *sum-numbers-range*
          collect (list (format nil "~a" k) k))
    (mk-dir (first x) :just
            (mk-pair-generator y
              (loop for k from 1 to *sum-numbers-range*
                    collect (list (format nil "~a" k)
                                  (+ k (second x))))
              (mk-file (first y)
                       (format nil "~a" (second y)))))))
