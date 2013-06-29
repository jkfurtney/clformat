from pprint import pprint
from collections import deque
import re
import int2num
from utility import int_to_roman, base10toN

# funny pep: http://www.python.org/dev/peps/pep-0313/


def parse_directive(control_string):
    """Input is a control string deque starting at a directive (with the ~
    already removed). The return value is a tuple of (directive_type,
    prefix_args, colon_modifier, at_modifier). The control_string
    deque is altered in place.

    """
    def parse_single_prefix_arg(control_string):
        """look for a single prefix argument. This can be v, #, a positive or
        integer or a single character prefixed by a ' (single quote)
        """
        s="".join(control_string)
        mobj = re.match(r"([+-]?[0-9]+|v|#|'.)",s)
        if mobj:
            arg=mobj.group()
            for i in range(len(arg)): control_string.popleft()
            return arg
        else:
            return None

    def parse_prefix_arg_list(control_string):
        "return list of prefix args"
        prefix_args=[None,None,None,None]
        arg = parse_single_prefix_arg(control_string)
        if arg: prefix_args[0]=arg
        for i in range(1,4):
            if control_string[0]==",":
                control_string.popleft()
                arg = parse_single_prefix_arg(control_string)
                if arg: prefix_args[i]=arg
        return prefix_args

    def parse_directive_modifiers(control_string):
        colon_modifier, at_modifier = False, False
        if control_string[0]==":" or control_string[0]=="@":
            if control_string.popleft()==":":
                colon_modifier=True
            else:
                at_modifier=True
        if control_string[0]==":" or control_string[0]=="@":
            if control_string.popleft()==":":
                if colon_modifier: raise Exception("More than one : in directive")
                colon_modifier=True
            else:
                if at_modifier: raise Exception("More than one @ in directive")
                at_modifier=True
        return colon_modifier, at_modifier

    def parse_directive_type(control_string):
        if control_string[0].isalnum():
            control_string[0]=control_string[0].lower()
        if control_string[0] in directive_list:
            return control_string.popleft()
        raise Exception("unknown directive type %s", control_string[0])

    prefix_args = parse_prefix_arg_list(control_string)
    colon_modifier, at_modifier  = parse_directive_modifiers(control_string)
    directive_type = parse_directive_type(control_string)
    return (directive_type, prefix_args, colon_modifier, at_modifier)


def tokenize(control_string):
    """Given the format control string return a list of tokens. Each token
    is either (i) a string or (ii) a tuple describing a directive (as
    returned by parse_directive())).

    """
    control_string=deque(control_string)
    out=['']
    while control_string:
        char = control_string.popleft()
        if char=="~":
            if out[-1]=='':
                out[-1] = parse_directive(control_string)
            else:
                out.append(parse_directive(control_string))
            out.append('')
        else:
            out[-1] += char
    return out

def build_tree(token_list):
    """Read the token list return by tokenize and build a tree
    representing the (possibly nested) directives and string


    """
    pair_directives = "[<({"
    compliment = {"[":"]", "{":"}", "(":")", "<":">"}


    class Node(object):
        """Represent a node in the syntax tree of the format language """
        def __init__(self, parent, value):
            self.parent = parent
            self.children = []
            self.value = value
        def add_child(self, child_value):
            self.children.append(Node(self, child_value))
            return self.children[-1]

    def build_tree(parent, token_list):
        """This function is called recursively to transform the token list
        into a tree. The paired directives created a nested tree of
        strings and directives. Here token_list is a deque.

        """
        while len(token_list):
            if type(token_list[0])==str:                        #  string node
                parent.add_child(token_list.popleft())
            else:                                       #  directive node
                char = token_list[0][0]
                if char in pair_directives:
                    current = parent.add_child(token_list.popleft())
                    # search forward for closing directive be carefull
                    # here as there may be nested directives of the
                    # same type
                    nested_count = 0
                    closing_char = compliment[char]
                    child_tokens = [] # collect children & call build_tree()
                    looking = True
                    while looking:
                        if len(token_list)==0:
                            raise Exception("End of format string before \
                                             finding closing %s" % closing_char)
                        if type(token_list[0])==str:            #  string node
                            child_tokens.append(token_list.popleft())
                        else:                           #  directive node
                            nested_char = token_list[0][0]
                            if nested_char == char:
                                nested_count += 1
                                child_tokens.append(token_list.popleft())
                            elif nested_char == closing_char:
                                if nested_count == 0:
                                    token_list.popleft()  #  drop the closing directives
                                    looking = False
                                else:
                                    child_tokens.append(token_list.popleft())
                                    nested_count -= 1
                            else:
                                child_tokens.append(token_list.popleft())
                    build_tree(current, deque(child_tokens))
                else:                                   #  normal directive
                    parent.add_child(token_list.popleft())

    top = Node(None, None)
    token_list = deque(token_list)
    build_tree(top,token_list)
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

class CLFormatter(object):
    """
    User facing
    """
    def __init__(self, control_string):
        self.control_string = control_string
        token_list = tokenize(control_string)
        self.tree = build_tree(token_list)

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
        return "<CLFormatter: %s>" % self.control_string

    def __call__(self, args):
        executor = FormatExecutor(self.tree, ArgumentList(args))
        return "".join(executor.output)

class FormatExecutor(object):
    def __init__(self, tree, args):
        self.tree = tree
        self.args = args
        self.output=[]
        self.process_node(self.tree, self.output)

    def process_node(self, node, output):
        """
        Return None to break out of directive
        """
        if type(node.value)==str:
            output.append(node.value)
        else:
            if node.value:       # process directive
                directive = node.value[0]
                if directive in directive_functions:
                    func = directive_functions[directive]
                    ret = func(node, self.args, self)
                    if ret is None: return None
                    output.append(ret)
                else:            # un implimented directive
                    output.append(directive)
            for child_node in node.children:
                ret = self.process_node(child_node, output)
                if ret is None: break
        return ""

def pre_process_prefix_args(prefix_args, args):
    """look for # or v as prefix args and replace them with the
    number of remaining args or pop an arg off. Returns a new
    prefix list.

    """
    local_prefix_args = [None,None,None,None]
    for i in range(len(prefix_args)):
        if prefix_args[i]=='#':
            local_prefix_args[i] = len(args)
        elif prefix_args[i]=='v':
            varg = self.args.popleft()
            assert type(varg)==int or type(varg)==str
            local_prefix_args[i] = varg
        else:
            local_prefix_args[i]=prefix_args[i]
    return local_prefix_args

# directive functions
# general directives: A and S
def a(node, args, executor):
    directive, _prefix_args, colon_modifier, at_modifier = node.value
    prefix_args = pre_process_prefix_args(_prefix_args, args)
    return str(args.popleft())

def s(node, args, executor):
    directive, _prefix_args, colon_modifier, at_modifier = node.value
    prefix_args = pre_process_prefix_args(_prefix_args, args)
    arg=args.popleft()
    if type(arg)==str:
        return '"%s"' % arg
    else:
        return str(arg)

# integer directives: d x b o and r
def d(node, args, executor):
    directive, _prefix_args, colon_modifier, at_modifier = node.value
    prefix_args = pre_process_prefix_args(_prefix_args, args)
    return "%i" % args.popleft()

def x(node, args, executor):
    directive, _prefix_args, colon_modifier, at_modifier = node.value
    prefix_args = pre_process_prefix_args(_prefix_args, args)
    return "%x" % args.popleft()

def b(node, args, executor):
    directive, _prefix_args, colon_modifier, at_modifier = node.value
    prefix_args = pre_process_prefix_args(_prefix_args, args)
    return bin(args.popleft())[2:]

def o(node, args, executor):
    directive, _prefix_args, colon_modifier, at_modifier = node.value
    prefix_args = pre_process_prefix_args(_prefix_args, args)
    return "%o" % args.popleft()

def r(node, args, executor):
    directive, _prefix_args, colon_modifier, at_modifier = node.value
    prefix_args = pre_process_prefix_args(_prefix_args, args)
    i = args.popleft()
    if not ((type(i) is int) or (type(i) is long)):
        raise ValueError("Argument to directive r must be an integer")
    if prefix_args[0] is None:
        if at_modifier:
            return int_to_roman(i)
        return int2num.spoken_number(i)
    else:
        radix = int(prefix_args[0])
        return base10toN(i, radix)

# floating point directives: E F G $
def e(node, args, executor):
    directive, _prefix_args, colon_modifier, at_modifier = node.value
    prefix_args = pre_process_prefix_args(_prefix_args, args)
    pass

def f(node, args, executor):
    directive, _prefix_args, colon_modifier, at_modifier = node.value
    prefix_args = pre_process_prefix_args(_prefix_args, args)
    pass

def g(node, args, executor):
    directive, _prefix_args, colon_modifier, at_modifier = node.value
    prefix_args = pre_process_prefix_args(_prefix_args, args)
    pass

def dollar(node, args, executor):
    directive, _prefix_args, colon_modifier, at_modifier = node.value
    prefix_args = pre_process_prefix_args(_prefix_args, args)
    pass

# special non-pair directives P ^ ~ % & *
def p(node, args, executor):
    directive, _prefix_args, colon_modifier, at_modifier = node.value
    prefix_args = pre_process_prefix_args(_prefix_args, args)

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

def circumflex(node, args, executor):  # ^
    directive, _prefix_args, colon_modifier, at_modifier = node.value
    prefix_args = pre_process_prefix_args(_prefix_args, args)

    if len(args)==0:
        return None
    else:
        return ""

def tilda(node, args, executor):
    directive, _prefix_args, colon_modifier, at_modifier = node.value
    prefix_args = pre_process_prefix_args(_prefix_args, args)

    if prefix_args[0] is None:        n=1
    else:                             n=prefix_args[0]
    return "~"*n

def percent(node, args, executor):
    directive, _prefix_args, colon_modifier, at_modifier = node.value
    prefix_args = pre_process_prefix_args(_prefix_args, args)
    return ""

def ampersand(node, args, executor):
    directive, _prefix_args, colon_modifier, at_modifier = node.value
    prefix_args = pre_process_prefix_args(_prefix_args, args)
    return "\n"

def asterisk(node, args, executor):
    directive, _prefix_args, colon_modifier, at_modifier = node.value
    prefix_args = pre_process_prefix_args(_prefix_args, args)

    if prefix_args[0] is None:        n=1
    else:                             n=prefix_args[0]
    if colon_modifier: args.rewind(n)
    elif at_modifier:
        args.goto(n)
    else:
        for i in range(n): args.popleft()

# pair directives [ { (
def conditional(node, args, executor):
    """
    Conditional directive ~[
    missing : and @ functionality
    """
    directive, _prefix_args, colon_modifier, at_modifier = node.value
    prefix_args = pre_process_prefix_args(_prefix_args, args)
    assert directive == "["
    if prefix_args[0] is None:
        index = args.popleft()
        assert type(index)==int
    else:
        index = prefix_args[0]
        assert type(index)==int
    current=0
    child_deque = deque(node.children[:])   #  a copy
    while child_deque:
        if current == index:  #  we found what we are looking for
            # process nodes until the next ~; or empty
            while True:
                cnode = child_deque.popleft()
                if type(cnode.value)==str:
                    executor.process_node(cnode, executor.output)
                elif cnode.value[0]==";":
                    break
                else:
                    ret = executor.process_node(cnode, executor.output)
                    if ret is None: return None
            return ""
        # looking for correct clause
        cnode = child_deque.popleft()
        if type(cnode.value)==str:
            pass
        elif cnode.value[0]==";":
            if cnode.value[2]:
                current = index
            else:
                current+=1
    return ""

def list_(node, args, executor):
    """Looping directive {. No : functionality. also fix to support ~:}
    which will process the body atleast once

    : argument should be a list of lists. Each pass over the
    children nodes should use one of the sublists as arg

    """
    directive, _prefix_args, colon_modifier, at_modifier = node.value
    prefix_args = pre_process_prefix_args(_prefix_args, args)
    assert directive == "{"
    if prefix_args[0] is None:
        max_iteration = 1e8
    else:
        max_iteration = prefix_args[0]
        assert type(max_iteration) == int

    if at_modifier:
        pass
    else:
        old_args = executor.args
        executor.args = deque(old_args.popleft())

    iteration = 0
    while args:
        if iteration >= max_iteration:
            break
        for child_node in node.children:
            ret = executor.process_node(child_node, executor.output)
            if ret is None:
                iteration = max_iteration
                break
        iteration += 1

    if at_modifier:
        pass
    else:
        executor.args = old_args
    return ""

def capitalization(node, args, executor):
    """
    (
    """
    directive, _prefix_args, colon_modifier, at_modifier = node.value
    assert directive=="("
    prefix_args = pre_process_prefix_args(_prefix_args, args)

    output = executor.output
    start_size = len(output)
    ret = ""
    for child in node.children:
        ret = executor.process_node(child, output)
        if ret is None: break

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


directive_functions = {'a':a, 'x':x, 'd':d, 'b':b, "~":tilda,
                       's':s, '%':percent, "o":o, 'r':r, "p":p,
                       "^":circumflex, "*":asterisk, "&": ampersand,
                       "[":conditional, "{":list_, "(":capitalization }

directive_list = "%&|t<>c()dboxrpfeg$as~<>{}[];^*"

def clformat(control_string, *args):
    formatter = CLFormatter(control_string)
    print formatter(args)


if __name__ == '__main__':
    assert parse_directive(deque(",,'.,4d"))==("d", [None, None, "'.", '4'], \
                                               False, False)
    assert parse_directive(deque("-2,-4:@x"))==("x", ['-2', '-4', None, None], \
                                                True, True)
    assert parse_directive(deque("v,'^,#,-302:@x"))==  \
        ("x", ['v',"'^","#",'-302'], True,True)
    assert parse_directive(deque("'2,-4:@x"))==("x", ["'2",'-4',None,None], True, \
                                                True)

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

    doctest.testmod(test, verbose=False)
    doctest.testmod(hyperspec_tests, verbose=False)
    doctest.testmod(pytests, verbose=False)
