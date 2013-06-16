def test():
    """ 
    >>> format("This is ~a ~%", "something")
    This is something 

    >>> format("String ~s in a string~%", "Simon")
    String "Simon" in a string

    >>> format("String ~a in a string~%", "Simon")
    String Simon in a string

    >>> format("This is ~10@a with lots of space ~%", 10)
    This is         10 with lots of space 

    >>> format("This is ~10a with lots of space ~%", 10)
    This is 10         with lots of space 

    >>> format("This is ~10,3a with lots of space ~%", 10)
    This is 10          with lots of space 

    >>> format("This word ~,,4,'!a is big~%", "james")
    This word james!!!! is big

    >>> format("This word ~@,,4,'!a is big~%", "james")
    This word !!!!james is big

    >>> format("~a in hex is ~x~%", 256, 256)
    256 in hex is 100

    >>> format("~a in binary is ~b~%", 42, 42)
    42 in binary is 101010

    >>> format("floats ~f ~%", 22.7)
    floats 22.7 

    >>> format("floats ~5f ~%", 22.777773)
    floats 22.778 

    >>> format("floats ~9e ~%", 22.777773)
    floats 2.27778e+1 

    >>> format("~r ~%", 12345)
    twelve thousand three hundred forty-five 

    >>> format("~{~a, ~}~%", (LIST 1 2 3))
    1, 2, 3, 

    >>> format("~{~a~^, ~}~%", (LIST 1 2 3))
    1, 2, 3

    >>> format("~{~a~#[~;, and ~:;, ~]~}", (LIST 1 2 3))
    1, 2, and 3
    >>> format("~{~#[~;~a~;~a and ~a~:;~@{~a~#[~;, and ~:;, ~]~}~]~}~%", (LIST))
    

    >>> format("~{~#[~;~a~;~a and ~a~:;~@{~a~#[~;, and ~:;, ~]~}~]~}~%", (LIST
                                                                          1))
    1

    >>> format("~{~#[~;~a~;~a and ~a~:;~@{~a~#[~;, and ~:;, ~]~}~]~}~%", (LIST
                                                                          1
                                                                          2))
    1 and 2

    >>> format("~{~#[~;~a~;~a and ~a~:;~@{~a~#[~;, and ~:;, ~]~}~]~}~%", (LIST
                                                                          1
                                                                          2
                                                                          3))
    1, 2, and 3

    >>> format("~{~#[~;~a~;~a and ~a~:;~@{~a~#[~;, and ~:;, ~]~}~]~}~%", (LIST
                                                                          1
                                                                          2
                                                                          3
                                                                          4))
    1, 2, 3, and 4

    >>> format("Done.~^ ~@(~R~) warning~:p.~^ ~@(~R~) error~:p.", 0, 0)
    Done. Zero warnings. Zero errors.
    >>> format("Done.~^ ~@(~R~) warning~:p.~^ ~@(~R~) error~:p.", 1, 0)
    Done. One warning. Zero errors.
    >>> format("Done.~^ ~@(~R~) warning~:p.~^ ~@(~R~) error~:p.", 1, 33)
    Done. One warning. Thirty-three errors.
    >>> format("Done.~^ ~@(~R~) warning~:p.~^ ~@(~R~) error~:p.", 33)
    Done. Thirty-three warnings.
    """ 