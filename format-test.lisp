(defmacro with-testfile-output (test-file-name &body body)
  "This macro creates a dummy Python function with doctest format
tests. The tests are defined by the format test-macro"
  `(with-open-file (fsc-tmp (namestring ,test-file-name)
                           :direction :output :if-exists :supersede)
     (let ((*standard-output* fsc-tmp))
       (format t "from clformat import clformat~%")
       (format t "def test():~%")
       (princ "    \"\"\" ")
       ,@body
       (princ "    \"\"\" "))))

; fix this to handle path to string and floats properly
(defun python-form (arg)
  "Given a lisp object of any type try to convert it into a form suitable for parsing by python. Does not process nested lists"
  (cond
    ((symbolp arg)
     (if arg               ; convert symbols to Strings
	 (format nil "~s" (symbol-name arg))
	 (format nil "[]")))
    ((listp arg) (format nil "[~{~s~^, ~}]" arg))
    (t (format nil "~s" arg))))

(defun format-test (fmt-string &rest fmt-args)
  "Wraps calls to format to create python tests. Each call to
format-test creates an individual Python test in doctest format. The
expected result is found by evaluating the format expression in lisp."
  (let ((result (format nil "~?" fmt-string fmt-args))
	(py-args (map 'list #'python-form fmt-args)))
    (fresh-line)
    (format t ">>> clformat(~s, ~{~a~^, ~})~%" fmt-string py-args)
    (princ result)
    (terpri)))

(with-testfile-output "test.py"
  (format-test "foo")
  (format-test "~5,'dd" 33)
  (format-test "~5,' d" 33)
  (format-test "~10,'_,'*,2:@d" 3443)
  (format-test "Just print a ~~ please.")
  (format-test "This is ~a " "something")
  (format-test "String ~s in a string~%" "Simon")
  (format-test "String ~a in a string~%" "Simon")
  (format-test "String ~a in a string~%" 'simon)
  (format-test "~{~&~vd~}" '(5 37 10 253 15 9847 10 559 5 12))
  (format-test "This is ~10@a with lots of space " 10)
  (format-test "This is ~10a with lots of space " 10)
  (format-test "This is ~10,3a with lots of space " 10)
  (format-test "This word ~,,4,'!a is big" "james")
  (format-test "This word ~,,4,'!@a is big~%" "james")

  (format-test "~a in hex is ~x~%" 256 256)
  (format-test "~a in binary is ~b~%" 42 42)
  (format-test "~a in octal is ~o~%" 42 42)

  (format-test "floats ~f ~%" 22.7)
  (format-test "floats ~5f ~%" 22.7777732747)
  (format-test "floats ~9e ~%" 22.7777732747)

  (format-test "~r ~%" 12345)

  (format-test "~{~a, ~}~%" (list 1 2 3))
  (format-test "~{~a~^, ~}~%" (list 1 2 3))

  (format-test "~{~a~#[~;, and ~:;, ~]~}" (list 1 2 3))

  (defparameter *english-list*
    "~{~#[~;~a~;~a and ~a~:;~@{~a~#[~;, and ~:;, ~]~}~]~}")
  (format-test *english-list* (list ))
  (format-test *english-list* (list 1))
  (format-test *english-list* (list 1 2))
  (format-test *english-list* (list 1 2 3))
  (format-test *english-list* (list 1 2 3 4))


  (format-test "~(~a~)" "sTring wiTH MiXed CAPs")
  (format-test "~@(~a~)" "sTring wiTH MiXed CAPs")
  (format-test "~:(~a~)" "sTring wiTH MiXed CAPs")
  (format-test "~:@(~a~)" "sTring wiTH MiXed CAPs")

  (defparameter *list-etc*
    "~#[NONE~;~a~;~a and ~a~:;~a, ~a~]~#[~; and ~a~:;, ~a, etc~].")
  (format-test *list-etc*)
  (format-test *list-etc* 1)
  (format-test *list-etc* 1 2)
  (format-test *list-etc* 1 2 3)
  (format-test *list-etc* 1 2 3 4)

  (defparameter *compiler-format-string*
    "Done.~^ ~@(~r~) warning~:p.~^ ~@(~r~) error~:p.")

  (format-test *compiler-format-string*)
  (format-test *compiler-format-string* 33)
  (format-test *compiler-format-string* 0 0)
  (format-test *compiler-format-string* 1 0)
  (format-test *compiler-format-string* 0 1)
  (format-test *compiler-format-string* 1 33)


  (format-test "~@R ~(~@R~)" 14 14)

  (format-test "~@(how is ~:(BOB SMITH~)?~)")
  )


(with-testfile-output
 "hyperspec_tests.py"

 (format nil "tests from the Common Lisp hyperspec:
	http://www.lispworks.com/documentation/HyperSpec/Body/22_ck.htm")

 (format-test "foo")
 (setq x 5)
 (format-test "The answer is ~D." x)
 (format-test "The answer is ~3D." x)
 (format-test "The answer is ~3,'0D." x)
 (format-test "The answer is ~:D." (expt 47 x))
 (setq y "elephant")
 (format-test "Look at the ~A!" y)
 (setq n 3)
 (format-test "~D item~:P found." n)
 (format-test "~R dog~:[s are~; is~] here." n (= n 1))

 (format-test "~R dog~:*~[s are~; is~:;s are~] here." n)

 (format-test "Here ~[are~;is~:;are~] ~:*~R pupp~:@P." n)


 (defun foo (x)
   (format-test "~6,2F|~6,2,1,'*F|~6,2,,'?F|~6F|~,2F|~F"
	   x x x x x x))
 (foo 3.14159)
 (foo -3.14159)
 (foo 100.0)
 (foo 1234.0)
 (foo 0.006)

 (defun foo (x)
   (format-test
	   "~9,2,1,,'*E|~10,3,2,2,'?,,'$E|~
            ~9,3,2,-2,'%@E|~9,2E"
	   x x x x))
 (foo 3.14159)
 (foo -3.14159)
 (foo 1100.0)
 (foo 1100.0L0)
 (foo 1.1E13)
 (foo 1.1L120)
 ;; (foo 1.1L1200)

 (format nil "As an example of the effects of varying the scale factor, the code")

 (dotimes (k 13)
   (format t "~%Scale factor ~2D: |~13,6,2,VE|"
	   (- k 5) (- k 5) 3.14159))


 ;; (defun foo (x)
 ;;   (format-test "~9,2,1,,'*g|~9,3,2,3,'?,,'$g|~9,3,2,0,'%g|~9,2g"
 ;; 	   x x x x))
 ;; (foo 0.0314159)
 ;; (foo 0.314159)
 ;; (foo 3.14159)
 ;; (foo 31.4159)
 ;; (foo 314.159)
 ;; (foo 3141.59)
 ;; (foo 3141.59L0)
 ;; (foo 3.14E12)
 ;; (foo 3.14L120)
 ;; (foo 3.14L1200)

 (format-test "~10<foo~;bar~>")
 (format-test "~10:<foo~;bar~>")
 (format-test "~10<foobar~>")
 (format-test "~10:<foobar~>")
 (format-test "~10:@<foo~;bar~>")
 (format-test "~10@<foobar~>")
 (format-test "~10:@<foobar~>")

 (format-test "Written to ~A." "foo.bin")

 (setq foo "Items:~#[ none~; ~A~; ~A and ~A~:;~@{~#[~; and~] ~A~^ ,~}~].")
 (format-test foo)
 (format-test foo 'foo)
 (format-test foo 'foo 'bar)
 (format-test foo 'foo 'bar 'baz)
 (format-test foo 'foo 'bar 'baz 'quux))
