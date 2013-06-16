(defmacro with-testfile-output (&body body)
  `(with-open-file (fsc-tmp (namestring "test.py")
                           :direction :output :if-exists :supersede)
     (let ((*standard-output* fsc-tmp))
       (format t "def test():~%")
       (princ "    \"\"\" ")
       ,@body
       (princ "    \"\"\" "))))

(defmacro myformat (fmt-string &body fmt-args)
  `(let ((result (format nil ,fmt-string ,@fmt-args)))
     (fresh-line)
     (format t "    >>> format(~s, ~{~s~^, ~})~%" ,fmt-string '(,@fmt-args))
     (princ (concatenate 'string "    " result))
     (terpri)))

(with-testfile-output
  (myformat "This is ~a ~%" "something")
  (myformat "String ~s in a string~%" "Simon")
  (myformat "String ~a in a string~%" "Simon")

  (myformat "This is ~10@a with lots of space ~%" 10)
  (myformat "This is ~10a with lots of space ~%" 10)
  (myformat "This is ~10,3a with lots of space ~%" 10)
  (myformat "This word ~,,4,'!a is big~%" "james")
  (myformat "This word ~@,,4,'!a is big~%" "james")

  (myformat "~a in hex is ~x~%" 256 256)
  (myformat "~a in binary is ~b~%" 42 42)
  (myformat "floats ~f ~%" 22.7)
  (myformat "floats ~5f ~%" 22.7777732747)
  (myformat "floats ~9e ~%" 22.7777732747)

  (myformat "~r ~%" 12345)

  (myformat "~{~a, ~}~%" (list 1 2 3))
  (myformat "~{~a~^, ~}~%" (list 1 2 3))

  (myformat "~{~a~#[~;, and ~:;, ~]~}" (list 1 2 3))

  (defparameter *english-list*
    "~{~#[~;~a~;~a and ~a~:;~@{~a~#[~;, and ~:;, ~]~}~]~}")

  (defparameter *my-english-list* (concatenate 'string *english-list* "~%"))

  (myformat *my-english-list* (list ))
  (myformat *my-english-list* (list 1))
  (myformat *my-english-list* (list 1 2))
  (myformat *my-english-list* (list 1 2 3))
  (myformat *my-english-list* (list 1 2 3 4))

  (defparameter *compiler-format-string*
    "Done.~^ ~@(~R~) warning~:p.~^ ~@(~R~) error~:p.")

  (myformat *compiler-format-string* 0 0)
  (myformat *compiler-format-string* 1 0)
  (myformat *compiler-format-string* 1 33)
  (myformat *compiler-format-string* 33)
  )
