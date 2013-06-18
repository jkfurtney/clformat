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
    "look for a single prefix argument"
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

    arg = parse_prefix_arg(a)
    if arg: prefix_args[0]=arg

    for i in range(1,4):
        if a[0]==",":
            a.popleft()
            arg = parse_prefix_arg(a)
            if arg: prefix_args[i]=arg

    return prefix_args

def parse_directive_modifiers(a):
    colon_modifier, at_modifier = False, False
    if a[0]==":" or a[0]=="@":
        if a.popleft()==":":
            colon_modifier=True
        else:
            at_modifier=True
    if a[0]==":" or a[0]=="@":
        if a.popleft()==":":
            if colon_modifier: raise Exception("More than one : in directive")
            colon_modifier=True
        else:
            if at_modifier: raise Exception("More than one @ in directive")
            at_modifier=True
    return colon_modifier, at_modifier

def parse_directive_type(a):
    if a[0] in "%&|t<>c()dboxrpfeg$as~":
        return a.popleft()
    raise Exception("unknown directive type %s", a[0])

def parse_directive(a):
    prefix_args = parse_prefix_args(a)
    colon_modifier, at_modifier  = parse_directive_modifiers(a)
    directive_type = parse_directive_type(a)
    return (prefix_args, colon_modifier, at_modifier, directive_type)

def clformat(control_string, *args):
    a=deque(control_string)
    out=['']
    while a:
        char = a.popleft()
        if char=="~":
            out.append(parse_directive(a))
            out.append("")
        else:
            out[-1] += char
    return out

if __name__ == '__main__':
    assert parse_prefix_args(deque(",,'.,4d"))==[None, None, "'.", '4']
    assert parse_prefix_args(deque("-2,-4:@x"))==['-2', '-4', None, None]
    assert parse_prefix_args(deque("v,'^,#,-302':@x"))==['v',"'^","#",'-302']
    print parse_prefix_args(deque("'2,-4:@x"))
    print parse_prefix_args(deque("x,-4:@x"))

    print clformat("~3,-4:@x", 10)
    print clformat("This is a hex number ~5x", 10)
    print clformat("~,,'.,4d", 36456096)
    print clformat("this is just a string")


    import doctest
    #doctest.testmod(test)
