

def format(control_string, *args):
    print control_string

    print args

if __name__ == '__main__':
    import doctest
    doctest.testfile("test.py")
