for x from (loop for i from 1 to 10 collect (list (fmt "~a" i) i))
file name (fmt "Number ~a~%" x)
