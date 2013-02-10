(in-package :query-fs)

(require :ironclad)

(defun base16-string-to-octet-array (s)
  (let* (
         (pairs 
          (iter:iter (iter:generate x :in-string s)
                     (iter:collect (list (iter:next x)
                                         (iter:next x)))))
         (pair-strings (mapcar (lambda (p)
                                       (map 'string
                                            'identity 
                                            p))
                               pairs))
         (octet-array 
          (make-array (length pair-strings) 
                      :element-type '(unsigned-byte 8)))
         )
        (iter:iter (iter:for p in pair-strings)
                   (iter:for n from 0)
                   (setf (aref octet-array n ) 
                         (parse-integer p :radix 16)))
        octet-array))

(defun octet-array-to-base16-string (a)
  (let* (
         (strings (map 'list 
                       (lambda (x) 
                               (format nil "~16,2,'0r" x))
                       a))
         (full-string (apply 'concatenate 'string strings))
         )
        full-string))

(defun crypto-key (password)
  (let* (
         (hasher (make-instance 'ironclad:pbkdf2 :digest :sha1))
         (key (ironclad:derive-key hasher 
                                   (cl-fuse::string-to-octets 
                                    (concatenate 'string "--" password) :direct)
                                   (make-array 8 :initial-element 32 :element-type '(unsigned-byte 8))
                                   1
                                   32)))
        key
        ))

(defun unencrypt-aes (x key)
  (ignore-errors 
   (let* (
          (cipher (ironclad:make-cipher :aes :key (crypto-key key) :mode :ctr 
                                        :initialization-vector (make-array 16 :element-type '(unsigned-byte 8)
                                                                           :initial-element 0)))
          (byte-arr (base16-string-to-octet-array x))
          (buffer (make-array (length byte-arr) :initial-element 32 :element-type '(unsigned-byte 8)))
          (dummy (ironclad:decrypt cipher byte-arr buffer))
          (output (cl-fuse::octets-to-string buffer :direct))
          )
         (declare (ignore dummy))
         output
         )))

(defun encrypt-aes (x key)
  (let* (
         (cipher (ironclad:make-cipher :aes :key (crypto-key key) :mode :ctr 
                                       :initialization-vector (make-array 16 :element-type '(unsigned-byte 8)
                                                                          :initial-element 0)))
         (byte-arr (cl-fuse::string-to-octets x :direct))
         (buffer (make-array (length byte-arr) :initial-element 32 :element-type '(unsigned-byte 8)))
         (dummy (ironclad:encrypt cipher byte-arr buffer))
         (output (octet-array-to-base16-string buffer))
         )
        (declare (ignore dummy))
        output
        ))

