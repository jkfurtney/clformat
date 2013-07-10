from pprint import pprint
from collections import deque
import re
import math
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
        mobj = re.match(r"([+-]?[0-9]+|V|v|#|'.)",s)
        if mobj:
            arg=mobj.group()
            for i in range(len(arg)): control_string.popleft()
            return arg
        else:
            return None

    def parse_prefix_arg_list(control_string):
        "return list of prefix args"
        prefix_args=[None,None,None,None,None,None,None] # up to 7 prefix args
        arg = parse_single_prefix_arg(control_string)
        if arg: prefix_args[0]=arg
        for i in range(1,7):
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
                if colon_modifier:
                    raise Exception("More than one : in directive")
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
        for child_node in tree.children:
            ret = self.process_node(child_node, self.output)
            if ret is None: break

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
        return ""


# directive helper functions

def pre_process_prefix_args(prefix_args, args):
    """look for # or v as prefix args and replace them with the
    number of remaining args or pop an arg off. Returns a new
    prefix list.

    """
    local_prefix_args = [None,None,None,None,None,None,None]
    for i in range(len(prefix_args)):
        if prefix_args[i]=='#':
            local_prefix_args[i] = len(args)
        elif prefix_args[i]=='v':
            varg = args.popleft()
            assert type(varg)==int or type(varg)==str
            local_prefix_args[i] = varg
        else:
            local_prefix_args[i]=prefix_args[i]
    return local_prefix_args

def get_prefix_int(arg, default=0):
    if arg is None: return default
    return int(arg)

def get_prefix_char(arg, default=" "):
    if arg is None: return default
    assert arg[0]=="'"
    return arg[1]

def pad_general(string, at_modifier, prefix_args):
    mincol = get_prefix_int(prefix_args[0])
    colinc = get_prefix_int(prefix_args[1], 1)
    minpad = get_prefix_int(prefix_args[2])
    pad_char = get_prefix_char(prefix_args[3])

    if len(string) < mincol or minpad>0:
        npad = int(math.ceil((mincol - len(string)) / colinc)) * colinc
        if npad<0: npad=0
        if len(string) + npad < mincol: npad+=colinc  # fix this
        npad += minpad
        if at_modifier:
            return pad_char*npad + string
        else:
            return string + pad_char*npad
    else:
        return string

# directive functions
# general directives: A and S
def a(node, args, executor):
    # no equiv for : modifier here.
    directive, _prefix_args, colon_modifier, at_modifier = node.value
    prefix_args = pre_process_prefix_args(_prefix_args, args)
    return pad_general(str(args.popleft()), at_modifier, prefix_args)


def s(node, args, executor):
    directive, _prefix_args, colon_modifier, at_modifier = node.value
    prefix_args = pre_process_prefix_args(_prefix_args, args)
    arg = args.popleft()
    if type(arg)==str:
        string = '"%s"' % arg
    else:
        string = str(arg)
    return pad_general(string, at_modifier, prefix_args)


#width, padchar, comma char, comma interval
def format_integer(number, string, at_modifier, colon_modifier, prefix_args):
    if colon_modifier:
        comma_char = get_prefix_char(prefix_args[2], ",")
        comma_interval = get_prefix_int(prefix_args[3], 3)
        assert comma_interval > 0
        def split_len(seq, length):
            return [seq[i:i+length] for i in range(0, len(seq), length)]
        groups = split_len(string[::-1], comma_interval)
        groups.reverse()
        groups = [group[::-1] for group in groups]
        string = comma_char.join(groups)
    if at_modifier:
        if number > 0:
            string = "+" + string

    # padding
    minimum_width = get_prefix_int(prefix_args[0])
    assert minimum_width >= 0
    pad_char = get_prefix_char(prefix_args[1])
    if len(string) < minimum_width:
        string = pad_char*(minimum_width-len(string)) + string
    return string

# integer directives: d x b o and r
def d(node, args, executor):
    directive, _prefix_args, colon_modifier, at_modifier = node.value
    prefix_args = pre_process_prefix_args(_prefix_args, args)
    number = args.popleft()
    return format_integer(number, "%i" % number, at_modifier,
                          colon_modifier, prefix_args)

def x(node, args, executor):
    directive, _prefix_args, colon_modifier, at_modifier = node.value
    prefix_args = pre_process_prefix_args(_prefix_args, args)
    number = args.popleft()
    return format_integer(number, "%x" % number, at_modifier,
                          colon_modifier, prefix_args)

def b(node, args, executor):
    directive, _prefix_args, colon_modifier, at_modifier = node.value
    prefix_args = pre_process_prefix_args(_prefix_args, args)
    number = args.popleft()
    return format_integer(number, bin(number)[2:], at_modifier,
                          colon_modifier, prefix_args)

def o(node, args, executor):
    directive, _prefix_args, colon_modifier, at_modifier = node.value
    prefix_args = pre_process_prefix_args(_prefix_args, args)
    number = args.popleft()
    return format_integer(number, "%o" % number, at_modifier,
                          colon_modifier, prefix_args)

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
    width = get_prefix_int(prefix_args[0])
    pad_char = get_prefix_char(prefix_args[5])
    return "%e" % float(args.popleft())

def f(node, args, executor):
    directive, _prefix_args, colon_modifier, at_modifier = node.value
    prefix_args = pre_process_prefix_args(_prefix_args, args)
    return "%f" % float(args.popleft())

def g(node, args, executor):
    directive, _prefix_args, colon_modifier, at_modifier = node.value
    prefix_args = pre_process_prefix_args(_prefix_args, args)
    "{g}".format(float(args.popleft()))

def dollar(node, args, executor):
    directive, _prefix_args, colon_modifier, at_modifier = node.value
    prefix_args = pre_process_prefix_args(_prefix_args, args)
    return "%.2f" % float(args.popleft())

# special non-pair directives P ^ ~ % & * t
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

    n = get_prefix_int(prefix_args[0], 1)
    return "~"*n

def percent(node, args, executor):
    directive, _prefix_args, colon_modifier, at_modifier = node.value
    prefix_args = pre_process_prefix_args(_prefix_args, args)
    return ""

def ampersand(node, args, executor):
    directive, _prefix_args, colon_modifier, at_modifier = node.value
    prefix_args = pre_process_prefix_args(_prefix_args, args)
    if executor.output == []:
        return ""
    elif executor.output[-1][-1] == "\n":
        return ""
    else: return "\n"

def asterisk(node, args, executor):
    directive, _prefix_args, colon_modifier, at_modifier = node.value
    prefix_args = pre_process_prefix_args(_prefix_args, args)

    n = get_prefix_int(prefix_args[0], 1)
    if colon_modifier: args.rewind(n)
    elif at_modifier:
        args.goto(n)
    else:
        for i in range(n): args.popleft()
    return ""

def get_current_column(output):
    column = 0
    # the strategy is to eat backwards in the output until we reach
    # the new line or exaust the output.
    for string in output[::-1]:
        for char in string[::-1]:
            if char == '\n':
                return column
            else:
                column += 1
    return column

def t(node, args, executor):
    "@ and : modifiers missing"
    directive, _prefix_args, colon_modifier, at_modifier = node.value
    prefix_args = pre_process_prefix_args(_prefix_args, args)
    #http://www.lispworks.com/documentation/HyperSpec/Body/22_cfa.htm
    column_number = get_prefix_int(prefix_args[0], 1)
    column_increment = get_prefix_int(prefix_args[1], 1)
    column = get_current_column(executor.output)
    if column < column_number:
        return " "*(column_number-column)


# pair directives [ { (
def conditional(node, args, executor):
    """
    Conditional directive ~[
    """
    directive, _prefix_args, colon_modifier, at_modifier = node.value
    prefix_args = pre_process_prefix_args(_prefix_args, args)
    assert directive == "["

    if prefix_args[0] is None:
        index = args.popleft()
    else:
        index = prefix_args[0]

    if at_modifier:
        if not index:
            return ""
    if colon_modifier:
        if index:
            index = 1
        else:
            index = 0

    current=0
    child_deque = deque(node.children[:])   #  a copy
    while child_deque:
        if current == index:  #  we found what we are looking for
            # process nodes until the next ~; or empty
            while True:
                if len(child_deque)==0: return ""
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
            if cnode.value[2]:          # colon modifier
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
    while executor.args:
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

def justification(node, args, executor):
    #just do nothing for now
    for child in node.children:
        ret = ""
        ret = executor.process_node(child, executor.output)
        if ret is None: break

    return ret

directive_functions = {'a':a, 'x':x, 'd':d, 'b':b, '~':tilda,
                       's':s, '%':percent, 'o':o, 'r':r, 'p':p,
                       'f':f, 'g':g, '$':dollar, 'e':e, 't':t,
                       "^":circumflex, "*":asterisk, "&": ampersand,
                       "[":conditional, "{":list_, "(":capitalization,
                       "<":justification}

directive_list = "%&|tcdboxrpfeg$as~<>{}[]();^*"

def clformat(control_string, *args):
    formatter = CLFormatter(control_string)
    print formatter(args)

def test():

    assert parse_directive(deque(",,'.,4d"))==("d", [None, None, "'.", '4',
                                                     None, None, None], \
                                               False, False)
    assert parse_directive(deque("-2,-4:@x"))==("x", ['-2', '-4', None, None,
                                                      None, None, None], \
                                                True, True)
    assert parse_directive(deque("v,'^,#,-302:@x"))==  \
        ("x", ['v',"'^","#",'-302', None, None, None], True,True)
    assert parse_directive(deque("'2,-4:@x"))==("x", ["'2",'-4',None,None,
                                                      None, None, None], True, \
                                                True)

    import doctest
    from tests import pytests, test, hyperspec_tests

    doctest.testmod(test, verbose=False)
    doctest.testmod(hyperspec_tests, verbose=False)
    doctest.testmod(pytests, verbose=False)
