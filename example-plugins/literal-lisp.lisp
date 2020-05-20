(in-package :query-fs)

(setf (gethash "cl" *query-loader-types*) :whole-file)
(setf (gethash "cl" *query-loaders*) 'read-to-progn)

(defun read-to-progn (f)
  `((mk-splice
      (progn
        ,@(loop with eof := (gensym)
                for x := (read f nil eof)
                while (not (eq x eof))
                collect x)))))
