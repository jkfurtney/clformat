from clformat import clformat
def test():
    """ 
>>> clformat("| ~{~<|~%| ~,33:;~2d ~>~}|", [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99])
|  0  1  2  3  4  5  6  7  8  9 |
| 10 11 12 13 14 15 16 17 18 19 |
| 20 21 22 23 24 25 26 27 28 29 |
| 30 31 32 33 34 35 36 37 38 39 |
| 40 41 42 43 44 45 46 47 48 49 |
| 50 51 52 53 54 55 56 57 58 59 |
| 60 61 62 63 64 65 66 67 68 69 |
| 70 71 72 73 74 75 76 77 78 79 |
| 80 81 82 83 84 85 86 87 88 89 |
| 90 91 92 93 94 95 96 97 98 99 |
>>> clformat("~5t~a ~15t~a ~25t~a~%", "cat", "iPhone", "iPhone")
     cat       iPhone    iPhone

>>> clformat("~5t~a ~15t~a ~25t~a~%", "rain", "pattern", "iPhone")
     rain      pattern   iPhone

>>> clformat("~5t~a ~15t~a ~25t~a~%", "iPhone", "rain", "stipulate")
     iPhone    rain      stipulate

>>> clformat("~5t~a ~15t~a ~25t~a~%", "stipulate", "pattern", "thunder")
     stipulate  pattern  thunder

>>> clformat("~30<~a~;~a~;~a~>~%", "rain", "pattern", "thunder")
rain      pattern      thunder

>>> clformat("~30<~a~;~a~;~a~>~%", "iPhone", "stipulate", "stipulate")
iPhone   stipulate   stipulate

>>> clformat("~30<~a~;~a~;~a~>~%", "pattern", "lightning", "iPhone")
pattern    lightning    iPhone

>>> clformat("~30<~a~;~a~;~a~>~%", "rain", "pattern", "rain")
rain       pattern        rain

>>> clformat("~30<~a~;~a~;~a~>~%", "rain", "stipulate", "cat")
rain       stipulate       cat

>>> clformat("~30<~a~;~a~;~a~>~%", "cat", "cat", "stipulate")
cat       cat        stipulate

>>> clformat("~30<~a~;~a~;~a~>~%", "lightning", "pattern", "cat")
lightning     pattern      cat

>>> clformat("~30<~a~;~a~;~a~>~%", "cat", "rain", "stipulate")
cat       rain       stipulate

>>> clformat("~30<~a~;~a~;~a~>~%", "pattern", "rain", "rain")
pattern       rain        rain

>>> clformat("~30<~a~;~a~;~a~>~%", "lightning", "rain", "lightning")
lightning    rain    lightning

>>> clformat("~12:@<~a~>~%~12:@<~a~>~%~12:@<~a~>~%", "pattern", "iPhone", "stipulate")
  pattern   
   iPhone   
 stipulate  

>>> clformat("~12:@<~a~>~%~12:@<~a~>~%~12:@<~a~>~%", "rain", "iPhone", "iPhone")
    rain    
   iPhone   
   iPhone   

>>> clformat("~12:@<~a~>~%~12:@<~a~>~%~12:@<~a~>~%", "rain", "thunder", "thunder")
    rain    
  thunder   
  thunder   

>>> clformat("~12:@<~a~>~%~12:@<~a~>~%~12:@<~a~>~%", "pattern", "cat", "iPhone")
  pattern   
    cat     
   iPhone   

>>> clformat("~12:@<~a~>~%~12:@<~a~>~%~12:@<~a~>~%", "pattern", "lightning", "lightning")
  pattern   
 lightning  
 lightning  

>>> clformat("~12:@<~a~>~%~12:@<~a~>~%~12:@<~a~>~%", "rain", "lightning", "lightning")
    rain    
 lightning  
 lightning  

>>> clformat("~12:@<~a~>~%~12:@<~a~>~%~12:@<~a~>~%", "thunder", "iPhone", "cat")
  thunder   
   iPhone   
    cat     

>>> clformat("~12:@<~a~>~%~12:@<~a~>~%~12:@<~a~>~%", "lightning", "iPhone", "lightning")
 lightning  
   iPhone   
 lightning  

>>> clformat("~12:@<~a~>~%~12:@<~a~>~%~12:@<~a~>~%", "rain", "rain", "iPhone")
    rain    
    rain    
   iPhone   

>>> clformat("~12:@<~a~>~%~12:@<~a~>~%~12:@<~a~>~%", "pattern", "iPhone", "iPhone")
  pattern   
   iPhone   
   iPhone   

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
            ~9,3,2,-2,'%@E|~9,2E", 1100.0d0, 1100.0d0, 1100.0d0, 1100.0d0)
  1.10d+3| 11.00$+02|%%%%%%%%%|  1.10d+3
>>> clformat("~9,2,1,,'*E|~10,3,2,2,'?,,'$E|~
            ~9,3,2,-2,'%@E|~9,2E", 1.1e13, 1.1e13, 1.1e13, 1.1e13)
*********| 11.00$+12|%%%%%%%%%| 1.10e+13
>>> clformat("~9,2,1,,'*E|~10,3,2,2,'?,,'$E|~
            ~9,3,2,-2,'%@E|~9,2E", 1.1d120, 1.1d120, 1.1d120, 1.1d120)
*********|??????????|%%%%%%%%%|1.10d+120

Scale factor -5: |.00000314159e+06|
Scale factor -4: |.000031416e+05|
Scale factor -3: |0.0003142e+04|
Scale factor -2: | 0.003142e+03|
Scale factor -1: | 0.031416e+02|
Scale factor  0: | 0.314159e+01|
Scale factor  1: | 3.141590e+00|
Scale factor  2: | 31.41590e-01|
Scale factor  3: | 314.1590e-02|
Scale factor  4: | 3141.590e-03|
Scale factor  5: | 31415.90e-04|
Scale factor  6: | 314159.0e-05|
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