from pprint import pprint
from collections import deque
import test
import re
import int2num

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
    if a[0] in "%&|t<>c()dboxrpfeg$as~<>{}[];^":
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
        """This function is called recursively to transform the token list
        into a tree. The paired directives created a nested tree of
        strings and directives.

        """
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

class CompiledCLFormatControlString(object):
    def __init__(self, tree, control_string):
        self.tree = tree
        self.control_string = control_string

    def print_tree(self):
        """
        for bebugging tree
        """
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

        print_node(self.tree, 0)
    def __repr__(self):
        return "CLFormat Compiles Control String: %s" % self.control_string

    def __call__(self, args):
        directive_functions = {'a':a, 'x':x, 'd':d, 'b':b, "~":tilda,
                               's':s, '%':percent, "o":o, 'r':r, "p":p,
                               "^":circumflex}
        args = ArgumentList(args)
        output=[]
        def process_node(node, output):
            if type(node.value)==str:
                output.append(node.value)
            else:
                if node.value:       # process directive
                    directive = node.value[0]
                    if directive in directive_functions:
                        func = directive_functions[directive]
                        _, prefix_args, colon_modifier,at_modifier = node.value
                        ret = func(prefix_args, colon_modifier,
                                   at_modifier, args)
                        output.append(ret)
                    else:
                        # un implimented directive
                        output.append(directive)
                for child_node in node.children:
                    process_node(child_node, output)

        process_node(self.tree, output)
        return output


class ArgumentList(object):
    """ Store and manipulate the argument list to clformat.
    """
    def __init__(self,args):
        self.data=deque(args)
        self.used_data = deque()
    def __len__(self):
        return len(self.data)
    def popleft(self):
        item = self.data.popleft()
        self.used_data.append(item)
        return item
    def empty(self):
        if len(self.data)==0: return True
        else: return False
    def rewind(self,n):
        assert type(n)==int and n>=0
        if len(self.used_data)<n:
            raise Exception("Can't rewind argument list, not enought arguments")
        for i in range(n):
            self.data.appendleft(self.used_data.pop())


# directive functions

def d(prefix_args, colon_modifier, at_modifier, args):
    """
    process directive d
    """
    return "%i" % args.popleft()

def a(prefix_args, colon_modifier, at_modifier, args):
    """
    """
    return str(args.popleft())

def s(prefix_args, colon_modifier, at_modifier, args):
    """
    """
    arg=args.popleft()
    if type(arg)==str:
        return '"%s"' % arg
    else:
        return str(arg)
def circumflex(prefix_args, colon_modifier, at_modifier, args):
    return "^"

def p(prefix_args, colon_modifier, at_modifier, args):
    if colon_modifier:   value = args.used_data[-1]
    else:                value = args.popleft()

    if at_modifier:
        single_suffix = "y"
        plural_suffix = "ies"
    else:
        single_suffix = ""
        plural_suffix = "s"

    if args.used_data[-1]==1: return single_suffix
    else:                     return plural_suffix

def x(prefix_args, colon_modifier, at_modifier, args):
    """
    """
    return "%x" % args.popleft()

def b(prefix_args, colon_modifier, at_modifier, args):
    """
    """
    return bin(args.popleft())[2:]

def o(prefix_args, colon_modifier, at_modifier, args):
    return "%o" % args.popleft()

def tilda(prefix_args, colon_modifier, at_modifier, args):
    return "~"

def percent(prefix_args, colon_modifier, at_modifier, args):
    return ""

def r(prefix_args, colon_modifier, at_modifier, args):
    return int2num.spoken_number(args.popleft())

def clformat(control_string, *args):
    token_list = tokenize(control_string)
    #pprint(token_list)
    tree = build_tree(token_list)
    cfmt = CompiledCLFormatControlString(tree, control_string)
    #cfmt.print_tree()
    #cfmt(args)
    #print cfmt
    ret = "".join(cfmt(args))
    print ret
    #return ret

if __name__ == '__main__':
    assert parse_prefix_arg_list(deque(",,'.,4d"))==[None, None, "'.", '4']
    assert parse_prefix_arg_list(deque("-2,-4:@x"))==['-2', '-4', None, None]
    assert parse_prefix_arg_list(deque("v,'^,#,-302':@x"))==  \
        ['v',"'^","#",'-302']
    assert parse_prefix_arg_list(deque("'2,-4:@x"))==["'2",'-4',None,None]


    clformat("~3,-4:@x", 101)
    clformat("0x0~3,-4:@x", 10350)
    clformat("This is a hex number ~5x", 10)
    clformat("~,,'.,4d", 36456096)
    clformat("this is just a string")
    clformat("Jason's cat: ~[Siamese~;Manx~;Persian~:;Alley~] Cat", 3)

    # clformat("~{~a~#[~;, and ~:;, ~]~}",  [1,2,3])
    #clformat("~{~#[~;~a~;~a and ~a~:;~@{~a~#[~;, and ~:;, ~]~}~]~}")


    import doctest
    doctest.testmod(test)
