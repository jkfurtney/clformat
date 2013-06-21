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

    """
