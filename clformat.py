from collections import deque
import test
import re

# directive, v, # special cases

def d(prefix_parameters, colon_modifier, at_modifier, argument,
      arg_list, arg_index):
    """
    process directive d
    """
# up to four prefix arguments
# ints dxobr

def parse_prefix_arg(a):
    s="".join(a)
    mobj = re.match(r"([+-]?[0-9]+|v|#|'.)",s)
    if mobj:
        arg=mobj.group()
        for i in range(len(arg)): a.popleft()
        return arg
    else:
        return None

def parse_prefix_args(a):
    "return list of prefix args"
    prefix_args=[None,None,None,None]

    arg0 = parse_prefix_arg(a)
    if arg0: prefix_args[0]=arg0

    if a[0]==",":
        a.popleft()
        arg1 = parse_prefix_arg(a)
        if arg1: prefix_args[1]=arg1
    if a[0]==",":
        a.popleft()
        arg2= parse_prefix_arg(a)
        if arg2: prefix_args[2] = arg2
    if a[0]==",":
        a.popleft()
        arg3= parse_prefix_arg(a)
        if arg3: prefix_args[3] = arg3
    return prefix_args

def parse_directive(a):
    prefix_args = parse_prefix_args(a)
    colon_modifier = False
    at_modifier = False

    s="".join(a)
    mobj=re.match(r"(:|@)(:|@)([dxobr])",s)
    print mobj.groups()

def clformat(control_string, *args):
    print control_string
    a=deque(control_string)
    out=[]
    while a:
        char = a.popleft()
        if char=="~":
            parse_directive(a)
        else:
            out.append(char)

    print args

if __name__ == '__main__':
    print parse_prefix_args(deque(",,'.,4d"))

    print parse_prefix_args(deque("-2,-4:@x"))
    print parse_prefix_args(deque("2,-4:@x"))
    print parse_prefix_args(deque("'2,-4:@x"))
    print parse_prefix_args(deque("x,-4:@x"))
    1/0
    clformat("~3,-4:@x")
    clformat("~5x")
    clformat("~,,'.,4d")


    import doctest
    #doctest.testmod(test)
