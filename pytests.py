from clformat import clformat

def test():
    """
    >>> clformat("Items: ~[Jason~;Kerry~;James~:;Simon~]",0)
    Items: Jason

    >>> clformat("Items: ~[Jason~;Kerry~;James~:;Simon~]",1)
    Items: Kerry

    >>> clformat("Items: ~[Jason~;Kerry~;James~:;Simon~]",2)
    Items: James

    >>> clformat("Items: ~[Jason~;Kerry~;James~:;Simon~]",99)
    Items: Simon

    >>> clformat("The lottery numbers are ~{~d ~}.", [85,33,40,23,89,93,29])
    The lottery numbers are 85 33 40 23 89 93 29 .

    >>> clformat("The lottery numbers are ~{~d~^, ~}.", [85,33,40,23,89,93,29])
    The lottery numbers are 85, 33, 40, 23, 89, 93, 29.

    >>> clformat("Items:~@{~#[~; and~] ~A~^,~}","a","b","c")
    Items: a, b, and c

    >>> clformat("~@(how is ~:(BOB SMITH~)?~)", )
    How is bob smith?

    >>> clformat("~R dog~:[s are~; is~] here.", 3, False)
    three dogs are here.

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

    >>> clformat("This is ~a", "something")
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

    >>> clformat("This is ~10@a with lots of space", 10)
    This is         10 with lots of space

    >>> clformat("This is ~10a with lots of space", 10)
    This is 10         with lots of space

    >>> clformat("This is ~10,3a with lots of space", 10)
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

    >>> clformat("~r~%", 12345)
    twelve thousand, three hundred forty-five

    >>> clformat("~{~a, ~}~%", [1, 2, 3]) # doctest: +NORMALIZE_WHITESPACE
    1, 2, 3,

    >>> clformat("~{~a~^, ~}~%", [1, 2, 3])
    1, 2, 3

    >>> clformat("~{~a~#[~;, and ~:;, ~]~}", [1, 2, 3])
    1, 2, and 3

    >>> clformat("~{~#[~;~a~;~a and ~a~:;~@{~a~#[~;, and ~:;, ~]~}~]~}", [])
    <BLANKLINE>

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

    >>> clformat("~3,,,4:@x", 101)
    +65

    >>> clformat("~3,,,4:@x", 104534350)
    +63b,114e

    >>> clformat("This is a hex number ~5x", 10)
    This is a hex number     a

    >>> clformat("~,,'.,4:d", 36456096)
    3645.6096

    >>> clformat("this is just a string")
    this is just a string

    >>> clformat("~R dog~:*~[s are~; is~:;s are~] here.", 3)
    three dogs are here.

    >>> clformat("~d ~:*~d", 1)
    1 1

    >>> clformat("~d ~2*~d", 1,2,3,4)
    1 4

    >>> clformat("foo", )
    foo

    >>> clformat("The answer is ~D.", 5)
    The answer is 5.

    >>> clformat("The answer is ~3D.", 5)
    The answer is   5.

    >>> clformat("The answer is ~3,'0D.", 5)
    The answer is 005.

    >>> clformat("The answer is ~:D.", 229345007)
    The answer is 229,345,007.


    """
