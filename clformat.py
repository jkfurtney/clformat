from pprint import pprint
from collections import deque
import re
import int2num
import roman

# funny pep: http://www.python.org/dev/peps/pep-0313/

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
    if a[0].isalnum():
        a[0]=a[0].lower()
    if a[0] in "%&|t<>c()dboxrpfeg$as~<>{}[];^*":
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
    def goto(self,n):
        assert type(n)==int and n>=0
        self.rewind(len(self.used_data))
        if n==0: return
        for i in range(n-1):
            self.popleft()


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
        return "CLFormat Compiled Control String: %s" % self.control_string

    def __call__(self, in_args):
        directive_functions = {'a':a, 'x':x, 'd':d, 'b':b, "~":tilda,
                               's':s, '%':percent, "o":o, 'r':r, "p":p,
                               "^":circumflex, "*":asterisk, "&": ampersand}
        self.args = ArgumentList(in_args)
        output=[]
        self.capitalization = False
        def pre_process_prefix_args(prefix_args):
            """look for # or v as prefix args and replace them with the
            number of remaining args or pop an arg off. Returns a new
            prefix list.

            """
            local_prefix_args = [None,None,None,None]
            for i in range(len(prefix_args)):
                if prefix_args[i]=='#':
                    local_prefix_args[i] = len(self.args)
                elif prefix_args[i]=='v':
                    varg = self.args.popleft()
                    assert type(varg)==int or type(varg)==str
                    local_prefix_args[i] = varg
                else:
                    local_prefix_args[i]=prefix_args[i]
            return local_prefix_args

        def process_conditional(node, output):
            """
            Conditional directive ~[
            missing : and @ functionality
            """
            directive, prefix_args, colon_modifier, at_modifier = node.value
            assert directive == "["
            local_prefix_args = pre_process_prefix_args(prefix_args)
            if local_prefix_args[0] is None:
                index = self.args.popleft()
                assert type(index)==int
            else:
                index = local_prefix_args[0]
                assert type(index)==int
            current=0
            child_deque = deque(node.children[:])   #  a copy
            while child_deque:
                if current == index:  #  we found what we are looking for
                    # process nodes until the next ~; or empty
                    while True:
                        if len(child_deque)==0:
                            return 1 # should never get here
                        cnode = child_deque.popleft()
                        if type(cnode.value)==str:
                            process_node(cnode, output)
                        elif cnode.value[0]==";":
                            break
                        else:
                            ret = process_node(cnode,output)
                            if ret is None: return None
                    return 1
                # looking for correct clause
                cnode = child_deque.popleft()
                if type(cnode.value)==str:
                    pass
                elif cnode.value[0]==";":
                    if cnode.value[2]:
                        current = index
                    else:
                        current+=1
            return 1

        def process_list(node, output):
            """Looping directive {. No : functionality. also fix to support ~:}
            which will process the body atleast once

            : argument should be a list of lists. Each pass over the
            children nodes should use one of the sublists as arg

            """
            directive, prefix_args, colon_modifier, at_modifier = node.value
            assert directive == "{"
            local_prefix_args = pre_process_prefix_args(prefix_args)
            if prefix_args[0] is None:
                max_iteration = 1e8
            else:
                max_iteration = prefix_args[0]
                assert type(max_iteration) == int

            if at_modifier:
                pass
            else:
                old_args = self.args
                self.args = deque(old_args.popleft())

            iteration = 0
            while self.args:
                if iteration >= max_iteration:
                    break
                for child_node in node.children:
                    ret = process_node(child_node, output)
                    if ret is None:
                        iteration=max_iteration
                        break
                iteration += 1

            if at_modifier:
                pass
            else:
                self.args=old_args


        def process_capitalization(node, output):
            """
            (
            """
            directive, prefix_args, colon_modifier, at_modifier = node.value
            assert directive=="("
            local_prefix_args = pre_process_prefix_args(prefix_args)

            self.capitalization=True
            start_size = len(output)
            ret = 1
            for child in node.children:
                ret = process_node(child, output)
                if ret is None: break
            self.capitalization=False

            # now retroactively do the capitalization
            case = 0
            if colon_modifier: case += 1
            if at_modifier:    case += 2

            if case == 0:                 # lower case all words
                for i in range(start_size,len(output)):
                    output[i] = output[i].lower()
            elif case == 1:               # capitalize first letter of each word
                for i in range(start_size,len(output)):
                    output[i] = output[i].title()
            elif case == 2: # capitalize the first word and lower case others.
                for i in range(start_size,len(output)):
                    if i == start_size:   #  find the first word
                        split_pattern = re.compile("(\s+|\S+)")
                        sub_list = split_pattern.findall(output[i])
                        found = False
                        for j in range(len(sub_list)):
                            if not found:
                                if re.match("\S+", sub_list[j]):
                                    sub_list[j] = sub_list[j].capitalize()
                                    found = True
                            else: sub_list[j] = sub_list[j].lower()
                        output[i] = "".join(sub_list)
                    else: output[i] = output[i].lower()

            elif case == 3:                # Upper case all words
                for i in range(start_size,len(output)):
                    output[i] = output[i].upper()
            else:
                raise Exception("should not get here")
            return ret

            # post process capitalization by altering the output list.

        def process_node(node, output):
            if type(node.value)==str:
                output.append(node.value)
            else:
                if node.value:       # process directive
                    directive = node.value[0]
                    if directive in directive_functions:
                        func = directive_functions[directive]
                        _, prefix_args, colon_modifier,at_modifier = node.value
                        local_prefix_args = pre_process_prefix_args(prefix_args)
                        ret = func(prefix_args, colon_modifier,
                                   at_modifier, self.args)
                        if ret is None: return None
                        output.append(ret)
                    elif directive=='[':
                        ret = process_conditional(node, output)
                        if ret is None: return None
                        return 1
                    elif directive=='{':
                        process_list(node, output)
                        return 1
                    elif directive=='(':
                        if self.capitalization:
                            pass
                        else:
                            ret = process_capitalization(node, output)
                            if ret is None: return None
                            return 1
                    else:
                        # un implimented directive
                        output.append(directive)
                for child_node in node.children:
                    ret = process_node(child_node, output)
                    if ret is None: break
            return 1

        process_node(self.tree, output)
        return output

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
    if len(args)==0:
        return None
    else:
        return ""

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
    if prefix_args[0] is None:        n=1
    else:                             n=prefix_args[0]
    return "~"*n

def percent(prefix_args, colon_modifier, at_modifier, args):
    return ""

def r(prefix_args, colon_modifier, at_modifier, args):
    i = args.popleft()
    if not type(i) is int:
        raise ValueError("Argument to directive r must be an integer")
    if at_modifier: return roman.int_to_roman(i)
    return int2num.spoken_number(i)

def ampersand(prefix_args, colon_modifier, at_modifier, args):
    return "\n"

def asterisk(prefix_args, colon_modifier, at_modifier, args):
    if prefix_args[0] is None:        n=1
    else:                             n=prefix_args[0]
    if colon_modifier: args.rewind(n)
    elif at_modifier:
        args.goto(n)
    else:
        for i in range(n): args.popleft()

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
    clformat("Jason's cat: ~[Siamese~;Manx~;Persian~:;Alley~] Cat", 2)

    # clformat("~{~a~#[~;, and ~:;, ~]~}",  [1,2,3])
    #clformat("~{~#[~;~a~;~a and ~a~:;~@{~a~#[~;, and ~:;, ~]~}~]~}")

    import doctest
    import test
    import hyperspec_tests
    import pytests

    doctest.testmod(test)
    doctest.testmod(hyperspec_tests)
    doctest.testmod(pytests)
