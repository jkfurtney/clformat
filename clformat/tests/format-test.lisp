(defpackage :format-test
  (:use :common-lisp)
  (:shadow :format))

(in-package :format-test)

(defun format (&rest args)
  (apply #'cl:format args))

(defmacro with-python-test-output (test-file-name &body body)
  "This macro creates a Python function with doctest tests. With in
the body of this macro all calls to FORMAT are made into Python tests
via the format-test function."
  `(with-open-file (fsc-tmp (namestring ,test-file-name)
                           :direction :output :if-exists :supersede)
     (let ((*standard-output* fsc-tmp))
       (format t "# do not edit this file. This file was automatically generated by format-test.lisp~%")
       (format t "from clformat import clformat~%")
       (format t "def test():~%")
       (princ "    \"\"\" ")
       (flet ((format (&rest args)
		      (apply #'format-test args)))
	 ,@body)
       (princ "    \"\"\" "))))

; fix this to handle path to string and floats properly
(defun python-form (arg)
  "Given a lisp object of any type try to convert it into a form
suitable for parsing by python. Does not process nested lists"
  (cond
    ((symbolp arg)
     (if arg               ; convert symbols to Strings
	 (format nil "~s" (symbol-name arg))
	 (format nil "[]")))
    ((listp arg) (format nil "[~{~s~^, ~}]" arg))
    (t (format nil "~s" arg))))

(defun format-test (_ fmt-string &rest fmt-args)
  "Wraps calls to format to create python tests. Each call to
format-test creates an individual Python test in doctest format. The
expected result is found by evaluating the format expression in lisp."
  (let ((result (format nil "~?" fmt-string fmt-args))
	(py-args (map 'list #'python-form fmt-args)))
    (fresh-line)
    (format t ">>> clformat(~s, ~{~a~^, ~})~%" fmt-string py-args)
    (princ result)
    (terpri)))

(with-python-test-output "test.py"

  (format t "| ~{~<|~%| ~,33:;~2d ~>~}|" (loop for x below 100 collect x))

  (defun random-word ()
    (nth (random 7) '("cat" "iPhone" "rain" "thunder" "lightning"
		      "stipulate" "pattern")))

  (dotimes (i 4)
    (format t "~5t~a ~15t~a ~25t~a~%"
		 (random-word) (random-word) (random-word)))

  (dotimes (i 10)
    (format t "~30<~a~;~a~;~a~>~%"
		 (random-word) (random-word) (random-word)))

  (dotimes (i 10)
    (format t "~12:@<~a~>~%~12:@<~a~>~%~12:@<~a~>~%"
		 (random-word) (random-word) (random-word))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(with-python-test-output
 "hyperspec_tests.py"

 (format nil "tests from the Common Lisp hyperspec: ~a~%"
	 "http://www.lispworks.com/documentation/HyperSpec/Body/22_ck.htm")

 (setq n 3)
 (format t "~D item~:P found." n)

 (defun foo (x)
   (format t "~6,2F|~6,2,1,'*F|~6,2,,'?F|~6F|~,2F|~F"
		x x x x x x))
 (foo 3.14159)
 (foo -3.14159)
 (foo 100.0)
 (foo 1234.0)
 (foo 0.006)

 (defun foo (x)
   (format t
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
 ;;   (format t "~9,2,1,,'*g|~9,3,2,3,'?,,'$g|~9,3,2,0,'%g|~9,2g"
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

 (format t "~10<foo~;bar~>")
 (format t "~10:<foo~;bar~>")
 (format t "~10<foobar~>")
 (format t "~10:<foobar~>")
 (format t "~10:@<foo~;bar~>")
 (format t "~10@<foobar~>")
 (format t "~10:@<foobar~>")

 (format t "Written to ~A." "foo.bin")

 (setq foo "Items:~#[ none~; ~A~; ~A and ~A~:;~@{~#[~; and~] ~A~^ ,~}~].")
 (format t foo)
 (format t foo 'foo)
 (format t foo 'foo 'bar)
 (format t foo 'foo 'bar 'baz)
 (format t foo 'foo 'bar 'baz 'quux))
