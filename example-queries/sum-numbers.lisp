with x from (loop for i from 1 to 20 collect (list (fmt "~a" i) i))
with y from (loop for i from 1 to 20 collect (list (fmt "~a" i) i))
file "sum" (fmt "Sum is ~a~%" (+ x y))
file "prod" (fmt "Product is ~a~%" (* x y))

