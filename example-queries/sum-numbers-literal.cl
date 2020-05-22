(defparameter *normal-lisp-code-p* t)

(mk-splice
  (mk-file "README" "This directory is a POSIX interface to #'CL:+")
  (mk-pair-generator x
    (loop with xn := (ignore-errors (parse-integer (first x)))
          for k in (if xn (list xn) (loop for x from 1 to 20 collect x))
          collect (list (format nil "~a" k) k))
    (mk-dir (first x) :just
            (mk-pair-generator
              y
              (loop with yn := (ignore-errors (parse-integer (first y)))
                    for k in (if yn (list yn) (loop for y from 1 to 20 collect y))
                    collect (list (format nil "~a" k) (+ k (second x))))
              (mk-file (first y) (format nil "~a" (second y)))))))
