import test

def clformat(control_string, *args):
    print control_string

    print args

if __name__ == '__main__':

    import doctest
    doctest.testmod(test)
