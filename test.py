from clformat import clformat
def test():
    """
>>> clformat("foo", )
foo
>>> clformat("~5,'dd", 33)
ddd33
>>> clformat("~5,' d", 33)
   33
>>> clformat("~10,'_,'*,2:@d", 3443)
____+34*43
>>> clformat("Just print a ~~ please.", )
Just print a ~ please.
>>> clformat("This is ~a ", "something")
This is something
>>> clformat("String ~s in a string~%", "Simon")
String "Simon" in a string

>>> clformat("String ~a in a string~%", "Simon")
String Simon in a string

>>> clformat("String ~a in a string~%", "SIMON")
String SIMON in a string

>>> clformat("~{~&~vd~}", [5, 37, 10, 253, 15, 9847, 10, 559, 5, 12])
   37
       253
           9847
       559
   12
>>> clformat("This is ~10@a with lots of space ", 10)
This is         10 with lots of space
>>> clformat("This is ~10a with lots of space ", 10)
This is 10         with lots of space
>>> clformat("This is ~10,3a with lots of space ", 10)
This is 10          with lots of space
>>> clformat("This word ~,,4,'!a is big", "james")
This word james!!!! is big
>>> clformat("This word ~,,4,'!@a is big~%", "james")
This word !!!!james is big

>>> clformat("~a in hex is ~x~%", 256, 256)
256 in hex is 100

>>> clformat("~a in binary is ~b~%", 42, 42)
42 in binary is 101010

>>> clformat("~a in octal is ~o~%", 42, 42)
42 in octal is 52

>>> clformat("floats ~f ~%", 22.7)
floats 22.7

>>> clformat("floats ~5f ~%", 22.777773)
floats 22.778

>>> clformat("floats ~9e ~%", 22.777773)
floats 2.27778e+1

>>> clformat("~r ~%", 12345)
twelve thousand three hundred forty-five

>>> clformat("~{~a, ~}~%", [1, 2, 3])
1, 2, 3,

>>> clformat("~{~a~^, ~}~%", [1, 2, 3])
1, 2, 3

>>> clformat("~{~a~#[~;, and ~:;, ~]~}", [1, 2, 3])
1, 2, and 3
>>> clformat("~{~#[~;~a~;~a and ~a~:;~@{~a~#[~;, and ~:;, ~]~}~]~}", [])

>>> clformat("~{~#[~;~a~;~a and ~a~:;~@{~a~#[~;, and ~:;, ~]~}~]~}", [1])
1
>>> clformat("~{~#[~;~a~;~a and ~a~:;~@{~a~#[~;, and ~:;, ~]~}~]~}", [1, 2])
1 and 2
>>> clformat("~{~#[~;~a~;~a and ~a~:;~@{~a~#[~;, and ~:;, ~]~}~]~}", [1, 2, 3])
1, 2, and 3
>>> clformat("~{~#[~;~a~;~a and ~a~:;~@{~a~#[~;, and ~:;, ~]~}~]~}", [1, 2, 3, 4])
1, 2, 3, and 4
>>> clformat("~(~a~)", "sTring wiTH MiXed CAPs")
string with mixed caps
>>> clformat("~@(~a~)", "sTring wiTH MiXed CAPs")
String with mixed caps
>>> clformat("~:(~a~)", "sTring wiTH MiXed CAPs")
String With Mixed Caps
>>> clformat("~:@(~a~)", "sTring wiTH MiXed CAPs")
STRING WITH MIXED CAPS
>>> clformat("~#[NONE~;~a~;~a and ~a~:;~a, ~a~]~#[~; and ~a~:;, ~a, etc~].", )
NONE.
>>> clformat("~#[NONE~;~a~;~a and ~a~:;~a, ~a~]~#[~; and ~a~:;, ~a, etc~].", 1)
1.
>>> clformat("~#[NONE~;~a~;~a and ~a~:;~a, ~a~]~#[~; and ~a~:;, ~a, etc~].", 1, 2)
1 and 2.
>>> clformat("~#[NONE~;~a~;~a and ~a~:;~a, ~a~]~#[~; and ~a~:;, ~a, etc~].", 1, 2, 3)
1, 2 and 3.
>>> clformat("~#[NONE~;~a~;~a and ~a~:;~a, ~a~]~#[~; and ~a~:;, ~a, etc~].", 1, 2, 3, 4)
1, 2, 3, etc.
>>> clformat("Done.~^ ~@(~r~) warning~:p.~^ ~@(~r~) error~:p.", )
Done.
>>> clformat("Done.~^ ~@(~r~) warning~:p.~^ ~@(~r~) error~:p.", 33)
Done. Thirty-three warnings.
>>> clformat("Done.~^ ~@(~r~) warning~:p.~^ ~@(~r~) error~:p.", 0, 0)
Done. Zero warnings. Zero errors.
>>> clformat("Done.~^ ~@(~r~) warning~:p.~^ ~@(~r~) error~:p.", 1, 0)
Done. One warning. Zero errors.
>>> clformat("Done.~^ ~@(~r~) warning~:p.~^ ~@(~r~) error~:p.", 0, 1)
Done. Zero warnings. One error.
>>> clformat("Done.~^ ~@(~r~) warning~:p.~^ ~@(~r~) error~:p.", 1, 33)
Done. One warning. Thirty-three errors.
>>> clformat("~@R ~(~@R~)", 14, 14)
XIV xiv
>>> clformat("~@(how is ~:(BOB SMITH~)?~)", )
How is bob smith?
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
>>> clformat("~5t~a ~15t~a ~25t~a~%", "lightning", "rain", "stipulate")
     lightning  rain     stipulate

>>> clformat("~5t~a ~15t~a ~25t~a~%", "thunder", "pattern", "stipulate")
     thunder   pattern   stipulate

>>> clformat("~5t~a ~15t~a ~25t~a~%", "lightning", "lightning", "thunder")
     lightning  lightning  thunder

>>> clformat("~5t~a ~15t~a ~25t~a~%", "iPhone", "thunder", "stipulate")
     iPhone    thunder   stipulate

>>> clformat("~30<~a~;~a~;~a~>~%", "iPhone", "lightning", "pattern")
iPhone    lightning    pattern

>>> clformat("~30<~a~;~a~;~a~>~%", "pattern", "pattern", "pattern")
pattern    pattern     pattern

>>> clformat("~30<~a~;~a~;~a~>~%", "thunder", "stipulate", "thunder")
thunder   stipulate    thunder

>>> clformat("~30<~a~;~a~;~a~>~%", "pattern", "thunder", "stipulate")
pattern   thunder    stipulate

>>> clformat("~30<~a~;~a~;~a~>~%", "rain", "thunder", "thunder")
rain      thunder      thunder

>>> clformat("~30<~a~;~a~;~a~>~%", "iPhone", "stipulate", "lightning")
iPhone   stipulate   lightning

>>> clformat("~30<~a~;~a~;~a~>~%", "stipulate", "cat", "pattern")
stipulate     cat      pattern

>>> clformat("~30<~a~;~a~;~a~>~%", "cat", "iPhone", "iPhone")
cat       iPhone        iPhone

>>> clformat("~30<~a~;~a~;~a~>~%", "iPhone", "lightning", "rain")
iPhone     lightning      rain

>>> clformat("~30<~a~;~a~;~a~>~%", "rain", "lightning", "pattern")
rain     lightning     pattern

>>> clformat("~12:@<~a~>~%~12:@<~a~>~%~12:@<~a~>~%", "rain", "cat", "lightning")
    rain
    cat
 lightning

>>> clformat("~12:@<~a~>~%~12:@<~a~>~%~12:@<~a~>~%", "thunder", "rain", "rain")
  thunder
    rain
    rain

>>> clformat("~12:@<~a~>~%~12:@<~a~>~%~12:@<~a~>~%", "pattern", "thunder", "cat")
  pattern
  thunder
    cat

>>> clformat("~12:@<~a~>~%~12:@<~a~>~%~12:@<~a~>~%", "pattern", "cat", "stipulate")
  pattern
    cat
 stipulate

>>> clformat("~12:@<~a~>~%~12:@<~a~>~%~12:@<~a~>~%", "cat", "thunder", "iPhone")
    cat
  thunder
   iPhone

>>> clformat("~12:@<~a~>~%~12:@<~a~>~%~12:@<~a~>~%", "pattern", "rain", "iPhone")
  pattern
    rain
   iPhone

>>> clformat("~12:@<~a~>~%~12:@<~a~>~%~12:@<~a~>~%", "thunder", "lightning", "thunder")
  thunder
 lightning
  thunder

>>> clformat("~12:@<~a~>~%~12:@<~a~>~%~12:@<~a~>~%", "lightning", "rain", "iPhone")
 lightning
    rain
   iPhone

>>> clformat("~12:@<~a~>~%~12:@<~a~>~%~12:@<~a~>~%", "lightning", "lightning", "rain")
 lightning
 lightning
    rain

>>> clformat("~12:@<~a~>~%~12:@<~a~>~%~12:@<~a~>~%", "cat", "rain", "thunder")
    cat
    rain
  thunder

>>> clformat("~36r", 3458734089537)
184x4z3nl
>>> clformat("~3r", 3458734089537)
110020122121010101112111110
>>> clformat("~,,':,2:x", 216737651708446)
c5:1f:2a:bd:52:1e
>>> clformat("~r", 4)
four
>>> clformat("~:r", 4)
fourth
>>> clformat("~@r", 4)
IV
>>> clformat("~:@r", 4)
IIII
    """
