
(defmacro myformat (fmt-string &body fmt-args)
  `(let ((result (format nil ,fmt-string ,@fmt-args)))
     (fresh-line)
     (prin1 ,fmt-string) (fresh-line)
     (prin1 ,@fmt-args) (fresh-line)
     (prin1 result)))

(myformat "This is ~a ~%" 'something)
(format t "String ~s in a string~%" "Simon")
(format t "String ~a in a string~%" "Simon")

(format t "This is ~10@a with lots of space ~%" 10)
(format t "This is ~10a with lots of space ~%" 10)
(format t "This is ~10,3a with lots of space ~%" 10)
(format t "This word ~,,4,'!a is big~%" "james")
(format t "This word ~@,,4,'!a is big~%" "james")

(format t "~a in hex is ~x~%" 256 256)
(format t "~a in binary is ~b~%" 42 42)
(format t "floats ~f ~%" 22.7)
(format t "floats ~5f ~%" 22.7777732747)
(format t "floats ~9e ~%" 22.7777732747)

(format t "~r ~%" 12345)

(format t "~{~a, ~}~%" (list 1 2 3))
(format t "~{~a~^, ~}~%" (list 1 2 3))

(format t "~{~a~#[~;, and ~:;, ~]~}" (list 1 2 3))

(defparameter *english-list*
  "~{~#[~;~a~;~a and ~a~:;~@{~a~#[~;, and ~:;, ~]~}~]~}")

(defparameter *my-english-list* (concatenate 'string *english-list* "~%"))

(format t *my-english-list* '())
(format t *my-english-list* '(1))
(format t *my-english-list* '(1 2))
(format t *my-english-list* '(1 2 3))
(format t *my-english-list* '(1 2 3 4))

(defvar *compiler-format-string*
  "Done.~^ ~@(~R~) warning~:p.~^ ~@(~R~) error~:p.~%")

(format t *compiler-format-string* 0 0)
(format t *compiler-format-string* 1 0)
(format t *compiler-format-string* 1 33)
(format t *compiler-format-string* 33)
