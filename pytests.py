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
    The lottery numbers are 85 33 40 23 89 93 29.

    >>> clformat("The lottery numbers are ~{~d~^, ~}.", [85,33,40,23,89,93,29])
    The lottery numbers are 85, 33, 40, 23, 89, 93, 29.


    """
