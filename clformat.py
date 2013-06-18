from pprint import pprint
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
    """look for a single prefix argument. This can be v, #, a positive or
    integer or a single character prefixed by a ' (single quote)
    """
    s="".join(a)
    mobj = re.match(r"([+-]?[0-9]+|v|#|'.)",s)
    if mobj:
        arg=mobj.group()
        for i in range(len(arg)): a.popleft()
        return arg
    else:
        return None

def parse_prefix_arg_list(a):
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
    if a[0] in "%&|t<>c()dboxrpfeg$as~<>{}[];":
        return a.popleft()
    raise Exception("unknown directive type %s", a[0])

def parse_directive(a):
    prefix_args = parse_prefix_arg_list(a)
    colon_modifier, at_modifier  = parse_directive_modifiers(a)
    directive_type = parse_directive_type(a)
    return (directive_type, prefix_args, colon_modifier, at_modifier)


def tokenize(control_string):
    a=deque(control_string)
    out=['']
    while a:
        char = a.popleft()
        if char=="~":
            if out[-1]=='':
                out[-1] = parse_directive(a)
            else:
                out.append(parse_directive(a))
            out.append('')
        else:
            out[-1] += char
    return out

class Node(object):
    """Represent a node in the syntax tree of the format language """
    def __init__(self, parent, value):
        self.parent = parent
        self.children = []
        self.value = value
    def add_child(self, child_value):
        self.children.append(Node(self, child_value))
        return self.children[-1]


def build_tree(token_list):
    pair_directives = "[<({"
    compliment = {"[":"]", "{":"}", "(":")", "<":">"}

    def build_tree(parent, tl):
        while len(tl):
            if type(tl[0])==str:                        #  string node
                parent.add_child(tl.popleft())
            else:                                       #  directive node
                char = tl[0][0]
                if char in pair_directives:
                    current = parent.add_child(tl.popleft())
                    # search forward for closing directive be carefull
                    # here as there may be nested directives of the
                    # same type
                    nested_count = 0
                    closing_char = compliment[char]
                    child_tokens = [] # collect children & call build_tree()
                    looking = True
                    while looking:
                        if len(tl)==0:
                            raise Exception("End of format string before \
                                             finding closing %s" % closing_char)
                        if type(tl[0])==str:            #  string node
                            child_tokens.append(tl.popleft())
                        else:                           #  directive node
                            nested_char = tl[0][0]
                            if nested_char == char:
                                nested_count += 1
                                child_tokens.append(tl.popleft())
                            elif nested_char == closing_char:
                                if nested_count == 0:
                                    tl.popleft()  #  drop the closing directives
                                    looking = False
                                else:
                                    child_tokens.append(tl.popleft())
                                    nested_count -= 1
                            else:
                                child_tokens.append(tl.popleft())
                    build_tree(current, deque(child_tokens))
                else:                                   #  normal directive
                    parent.add_child(tl.popleft())

    top = Node(None, None)
    tl = deque(token_list)
    build_tree(top,tl)
    return top

def print_tree(tree):
    def print_node(node, depth):
        print depth,
        if node.value is None:
            print "Top"
        elif type(node.value)==str:
            print "'%s'" % (node.value[:60])
        else:
            print "<directive %s>" % (node.value[0],)
        for child_node in node.children:
            print_node(child_node,depth+1)

    print_node(tree, 0)

def clformat(control_string, *args):
    # out is not a list of strings and tuples representing
    token_list = tokenize(control_string)
    pprint(token_list)
    tree = build_tree(token_list)
    print_tree(tree)
    return tree

if __name__ == '__main__':


    assert parse_prefix_arg_list(deque(",,'.,4d"))==[None, None, "'.", '4']
    assert parse_prefix_arg_list(deque("-2,-4:@x"))==['-2', '-4', None, None]
    assert parse_prefix_arg_list(deque("v,'^,#,-302':@x"))==  \
        ['v',"'^","#",'-302']
    assert parse_prefix_arg_list(deque("'2,-4:@x"))==["'2",'-4',None,None]


    print clformat("~3,-4:@x", 10)
    print clformat("This is a hex number ~5x", 10)
    print clformat("~,,'.,4d", 36456096)
    print clformat("this is just a string")
    clformat("Jason's cat: ~[Siamese~;Manx~;Persian~:;Alley~] Cat", 3)

    clformat("~{~a~#[~;, and ~:;, ~]~}", [1,2,3])
    clformat("~{~#[~;~a~;~a and ~a~:;~@{~a~#[~;, and ~:;, ~]~}~]~}")


    import doctest
    #doctest.testmod(test)
