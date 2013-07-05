from clformat import clformat
def test():
    """ 
>>> clformat("tests from the Common Lisp hyperspec: ~a~%", "http://www.lispworks.com/documentation/HyperSpec/Body/22_ck.htm")
tests from the Common Lisp hyperspec: http://www.lispworks.com/documentation/HyperSpec/Body/22_ck.htm

>>> clformat("~D item~:P found.", 3)
3 items found.
>>> clformat("~6,2F|~6,2,1,'*F|~6,2,,'?F|~6F|~,2F|~F", 3.14159, 3.14159, 3.14159, 3.14159, 3.14159, 3.14159)
  3.14| 31.42|  3.14|3.14159|3.14|3.14159
>>> clformat("~6,2F|~6,2,1,'*F|~6,2,,'?F|~6F|~,2F|~F", -3.14159, -3.14159, -3.14159, -3.14159, -3.14159, -3.14159)
 -3.14|-31.42| -3.14|-3.1416|-3.14|-3.14159
>>> clformat("~6,2F|~6,2,1,'*F|~6,2,,'?F|~6F|~,2F|~F", 100.0, 100.0, 100.0, 100.0, 100.0, 100.0)
100.00|******|100.00| 100.0|100.00|100.0
>>> clformat("~6,2F|~6,2,1,'*F|~6,2,,'?F|~6F|~,2F|~F", 1234.0, 1234.0, 1234.0, 1234.0, 1234.0, 1234.0)
1234.00|******|??????|1234.0|1234.00|1234.0
>>> clformat("~6,2F|~6,2,1,'*F|~6,2,,'?F|~6F|~,2F|~F", 0.006, 0.006, 0.006, 0.006, 0.006, 0.006)
  0.01|  0.06|  0.01| 0.006|0.01|0.006
>>> clformat("~9,2,1,,'*E|~10,3,2,2,'?,,'$E|~
            ~9,3,2,-2,'%@E|~9,2E", 3.14159, 3.14159, 3.14159, 3.14159)
  3.14e+0| 31.42$-01|%%%%%%%%%|  3.14e+0
>>> clformat("~9,2,1,,'*E|~10,3,2,2,'?,,'$E|~
            ~9,3,2,-2,'%@E|~9,2E", -3.14159, -3.14159, -3.14159, -3.14159)
 -3.14e+0|-31.42$-01|%%%%%%%%%| -3.14e+0
>>> clformat("~9,2,1,,'*E|~10,3,2,2,'?,,'$E|~
            ~9,3,2,-2,'%@E|~9,2E", 1100.0, 1100.0, 1100.0, 1100.0)
  1.10e+3| 11.00$+02|%%%%%%%%%|  1.10e+3
>>> clformat("~9,2,1,,'*E|~10,3,2,2,'?,,'$E|~
            ~9,3,2,-2,'%@E|~9,2E", 1100.0l0, 1100.0l0, 1100.0l0, 1100.0l0)
  1.10l+3| 11.00$+02|%%%%%%%%%|  1.10l+3
>>> clformat("~9,2,1,,'*E|~10,3,2,2,'?,,'$E|~
            ~9,3,2,-2,'%@E|~9,2E", 1.1e13, 1.1e13, 1.1e13, 1.1e13)
*********| 11.00$+12|%%%%%%%%%| 1.10e+13
>>> clformat("~9,2,1,,'*E|~10,3,2,2,'?,,'$E|~
            ~9,3,2,-2,'%@E|~9,2E", 1.1l120, 1.1l120, 1.1l120, 1.1l120)
*********|??????????|%%%%%%%%%|1.10l+120
>>> clformat("As an example of the effects of varying the scale factor, the code", )
As an example of the effects of varying the scale factor, the code
>>> clformat("~%Scale factor ~2D: |~13,6,2,VE|", -5, -5, 3.14159)

Scale factor -5: |.00000314159e+06|
>>> clformat("~%Scale factor ~2D: |~13,6,2,VE|", -4, -4, 3.14159)

Scale factor -4: |.000031416e+05|
>>> clformat("~%Scale factor ~2D: |~13,6,2,VE|", -3, -3, 3.14159)

Scale factor -3: |0.0003142e+04|
>>> clformat("~%Scale factor ~2D: |~13,6,2,VE|", -2, -2, 3.14159)

Scale factor -2: | 0.003142e+03|
>>> clformat("~%Scale factor ~2D: |~13,6,2,VE|", -1, -1, 3.14159)

Scale factor -1: | 0.031416e+02|
>>> clformat("~%Scale factor ~2D: |~13,6,2,VE|", 0, 0, 3.14159)

Scale factor  0: | 0.314159e+01|
>>> clformat("~%Scale factor ~2D: |~13,6,2,VE|", 1, 1, 3.14159)

Scale factor  1: | 3.141590e+00|
>>> clformat("~%Scale factor ~2D: |~13,6,2,VE|", 2, 2, 3.14159)

Scale factor  2: | 31.41590e-01|
>>> clformat("~%Scale factor ~2D: |~13,6,2,VE|", 3, 3, 3.14159)

Scale factor  3: | 314.1590e-02|
>>> clformat("~%Scale factor ~2D: |~13,6,2,VE|", 4, 4, 3.14159)

Scale factor  4: | 3141.590e-03|
>>> clformat("~%Scale factor ~2D: |~13,6,2,VE|", 5, 5, 3.14159)

Scale factor  5: | 31415.90e-04|
>>> clformat("~%Scale factor ~2D: |~13,6,2,VE|", 6, 6, 3.14159)

Scale factor  6: | 314159.0e-05|
>>> clformat("~%Scale factor ~2D: |~13,6,2,VE|", 7, 7, 3.14159)

Scale factor  7: | 3141590.e-06|
>>> clformat("~10<foo~;bar~>", )
foo    bar
>>> clformat("~10:<foo~;bar~>", )
  foo  bar
>>> clformat("~10<foobar~>", )
    foobar
>>> clformat("~10:<foobar~>", )
    foobar
>>> clformat("~10:@<foo~;bar~>", )
 foo bar  
>>> clformat("~10@<foobar~>", )
foobar    
>>> clformat("~10:@<foobar~>", )
  foobar  
>>> clformat("Written to ~A.", "foo.bin")
Written to foo.bin.
>>> clformat("Items:~#[ none~; ~A~; ~A and ~A~:;~@{~#[~; and~] ~A~^ ,~}~].", )
Items: none.
>>> clformat("Items:~#[ none~; ~A~; ~A and ~A~:;~@{~#[~; and~] ~A~^ ,~}~].", "FOO")
Items: FOO.
>>> clformat("Items:~#[ none~; ~A~; ~A and ~A~:;~@{~#[~; and~] ~A~^ ,~}~].", "FOO", "BAR")
Items: FOO and BAR.
>>> clformat("Items:~#[ none~; ~A~; ~A and ~A~:;~@{~#[~; and~] ~A~^ ,~}~].", "FOO", "BAR", "BAZ")
Items: FOO , BAR , and BAZ.
>>> clformat("Items:~#[ none~; ~A~; ~A and ~A~:;~@{~#[~; and~] ~A~^ ,~}~].", "FOO", "BAR", "BAZ", "QUUX")
Items: FOO , BAR , BAZ , and QUUX.
    """ 