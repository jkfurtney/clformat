(defmacro with-testfile-output (&body body)
  "This macro creates a dummy Python function with doctest format
tests. The tests are defined by the format test-macro"
  `(with-open-file (fsc-tmp (namestring "test.py")
                           :direction :output :if-exists :supersede)
     (let ((*standard-output* fsc-tmp))
       (format t "from clformat import clformat~%")
       (format t "def test():~%")
       (princ "    \"\"\" ")
       ,@body
       (princ "    \"\"\" "))))

(defmacro format-test (fmt-string &body fmt-args)
  "Wraps calls to format to create python tests. Each call to
format-test creates an individual Python test. The expected result is
found by evaluating the format expression in lisp."
  `(let ((result (format nil ,fmt-string ,@fmt-args)))
     (fresh-line)
     (format t ">>> clformat(~s, ~{~s~^, ~})~%" ,fmt-string '(,@fmt-args))
     (princ result)
     (terpri)))

(with-testfile-output
  (format-test "foo")
  (format-test "~5,'dd" 33)
  (format-test "~5,' d" 33)
  (format-test "~10,'_,'*,2:@d" 3443)
  (format-test "Just print a ~~ please.")
  (format-test "This is ~a ~%" "something")
  (format-test "String ~s in a string~%" "Simon")
  (format-test "String ~a in a string~%" "Simon")
  (format-test "~{~&~vd~}" '(5 37 10 253 15 9847 10 559 5 12))
  (format-test "This is ~10@a with lots of space ~%" 10)
  (format-test "This is ~10a with lots of space ~%" 10)
  (format-test "This is ~10,3a with lots of space ~%" 10)
  (format-test "This word ~,,4,'!a is big~%" "james")
  (format-test "This word ~@,,4,'!a is big~%" "james")

  (format-test "~a in hex is ~x~%" 256 256)
  (format-test "~a in binary is ~b~%" 42 42)
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
  (format-test *list-etc* 1)
  (format-test *list-etc* 1 2)
  (format-test *list-etc* 1 2 3)
  (format-test *list-etc* 1 2 3 4)

  (defparameter *compiler-format-string*
    "Done.~^ ~@(~r~) warning~:p.~^ ~@(~r~) error~:p.")

  (format-test *compiler-format-string* 0 0)
  (format-test *compiler-format-string* 1 0)
  (format-test *compiler-format-string* 1 33)
  (format-test *compiler-format-string* 33))

; tests from hyperspec
; http://www.lispworks.com/documentation/HyperSpec/Body/22_ck.htm

;; (format nil "foo") =>  "foo"
;; (setq x 5) =>  5
;; (format nil "The answer is ~D." x) =>  "The answer is 5."
;;  (format nil "The answer is ~3D." x) =>  "The answer is   5."
;;  (format nil "The answer is ~3,'0D." x) =>  "The answer is 005."
;;  (format nil "The answer is ~:D." (expt 47 x))
;; =>  "The answer is 229,345,007."
;;  (setq y "elephant") =>  "elephant"
;;  (format nil "Look at the ~A!" y) =>  "Look at the elephant!"
;;  (setq n 3) =>  3
;;  (format nil "~D item~:P found." n) =>  "3 items found."
;;  (format nil "~R dog~:[s are~; is~] here." n (= n 1))
;; =>  "three dogs are here."
;;  (format nil "~R dog~:*~[s are~; is~:;s are~] here." n)
;; =>  "three dogs are here."
;;  (format nil "Here ~[are~;is~:;are~] ~:*~R pupp~:@P." n)
;; =>  "Here are three puppies."

;;  (defun foo (x)
;;    (format nil "~6,2F|~6,2,1,'*F|~6,2,,'?F|~6F|~,2F|~F"
;;            x x x x x x)) =>  FOO
;;  (foo 3.14159)  =>  "  3.14| 31.42|  3.14|3.1416|3.14|3.14159"
;;  (foo -3.14159) =>  " -3.14|-31.42| -3.14|-3.142|-3.14|-3.14159"
;;  (foo 100.0)    =>  "100.00|******|100.00| 100.0|100.00|100.0"
;;  (foo 1234.0)   =>  "1234.00|******|??????|1234.0|1234.00|1234.0"
;;  (foo 0.006)    =>  "  0.01|  0.06|  0.01| 0.006|0.01|0.006"

;;  (defun foo (x)
;;     (format nil
;;            "~9,2,1,,'*E|~10,3,2,2,'?,,'$E|~
;;             ~9,3,2,-2,'%@E|~9,2E"
;;            x x x x))
;;  (foo 3.14159)  =>  "  3.14E+0| 31.42$-01|+.003E+03|  3.14E+0"
;;  (foo -3.14159) =>  " -3.14E+0|-31.42$-01|-.003E+03| -3.14E+0"
;;  (foo 1100.0)   =>  "  1.10E+3| 11.00$+02|+.001E+06|  1.10E+3"
;;  (foo 1100.0L0) =>  "  1.10L+3| 11.00$+02|+.001L+06|  1.10L+3"
;;  (foo 1.1E13)   =>  "*********| 11.00$+12|+.001E+16| 1.10E+13"
;;  (foo 1.1L120)  =>  "*********|??????????|%%%%%%%%%|1.10L+120"
;;  (foo 1.1L1200) =>  "*********|??????????|%%%%%%%%%|1.10L+1200"

;; As an example of the effects of varying the scale factor, the code

;;  (dotimes (k 13)
;;    (format t "~%Scale factor ~2D: |~13,6,2,VE|"
;;            (- k 5) (- k 5) 3.14159))

;; produces the following output:

;; Scale factor -5: | 0.000003E+06|
;; Scale factor -4: | 0.000031E+05|
;; Scale factor -3: | 0.000314E+04|
;; Scale factor -2: | 0.003142E+03|
;; Scale factor -1: | 0.031416E+02|
;; Scale factor  0: | 0.314159E+01|
;; Scale factor  1: | 3.141590E+00|
;; Scale factor  2: | 31.41590E-01|
;; Scale factor  3: | 314.1590E-02|
;; Scale factor  4: | 3141.590E-03|
;; Scale factor  5: | 31415.90E-04|
;; Scale factor  6: | 314159.0E-05|
;; Scale factor  7: | 3141590.E-06|

;;  (defun foo (x)
;;    (format nil "~9,2,1,,'*G|~9,3,2,3,'?,,'$G|~9,3,2,0,'%G|~9,2G"
;;           x x x x))
;;  (foo 0.0314159) =>  "  3.14E-2|314.2$-04|0.314E-01|  3.14E-2"
;;  (foo 0.314159)  =>  "  0.31   |0.314    |0.314    | 0.31    "
;;  (foo 3.14159)   =>  "   3.1   | 3.14    | 3.14    |  3.1    "
;;  (foo 31.4159)   =>  "   31.   | 31.4    | 31.4    |  31.    "
;;  (foo 314.159)   =>  "  3.14E+2| 314.    | 314.    |  3.14E+2"
;;  (foo 3141.59)   =>  "  3.14E+3|314.2$+01|0.314E+04|  3.14E+3"
;;  (foo 3141.59L0) =>  "  3.14L+3|314.2$+01|0.314L+04|  3.14L+3"
;;  (foo 3.14E12)   =>  "*********|314.0$+10|0.314E+13| 3.14E+12"
;;  (foo 3.14L120)  =>  "*********|?????????|%%%%%%%%%|3.14L+120"
;;  (foo 3.14L1200) =>  "*********|?????????|%%%%%%%%%|3.14L+1200"

;;  (format nil "~10<foo~;bar~>")   =>  "foo    bar"
;;  (format nil "~10:<foo~;bar~>")  =>  "  foo  bar"
;;  (format nil "~10<foobar~>")     =>  "    foobar"
;;  (format nil "~10:<foobar~>")    =>  "    foobar"
;;  (format nil "~10:@<foo~;bar~>") =>  "  foo bar "
;;  (format nil "~10@<foobar~>")    =>  "foobar    "
;;  (format nil "~10:@<foobar~>")   =>  "  foobar  "

;;   (FORMAT NIL "Written to ~A." #P"foo.bin")
;;   =>  "Written to foo.bin."

; errors (format "~5a" (make-hash-table)) ; Cannot output to a non adjustable string
