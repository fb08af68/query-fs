(in-package :query-fs)

(require :cl-utilities)

(defun basename (path)
  (car (last (cl-utilities:split-sequence 
              #\/ 
              (if (stringp path) path (car path))))))
