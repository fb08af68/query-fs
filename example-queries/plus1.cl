(mk-pair-generator x
  (let ((xn (ignore-errors (parse-integer (first x)))))
    (if xn `((,(first x) ,(1+ xn)))
      (loop for k from 1 to 10 collect `(,(format nil "~a" k) ,(1+ k)))))
  (mk-file (first x) (format nil "~a" (second x))))
