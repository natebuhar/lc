
import re, enum

class Reader(enum.Enum):
    Open  = 0
    Close = 1

def compile_readtable(readtable):
    pattern = []
    for p, r in readtable:
        pattern.append('(' + p + ')')
    return re.compile('|'.join(pattern))

def lex(string, readtable):
    token   = ''
    pattern = compile_readtable(readtable)

    def matches():
        nonlocal token
        i = 0
        while i <= len(token):
            match = pattern.match(token[i:])
            if match:
                e = match.span()[1]
                l = i + e
                if i > 0:
                    yield token[:i], None
                yield token[i:l], match.lastindex - 1
                token = token[l:]
                i     = 0
            else:
                i += 1
        if len(token) > 0:
            yield token, None
            token = ''

    for c in string:
        if not c.isspace():
            token += c
        else:
            yield from matches()

    yield from matches()

def parse_tail(head, tail, readtable, paren):
    token, r = head
    reader   = None if r is None else readtable[r][1]

    if reader is None:
        return token

    elif reader == Reader.Open:
        paren.append(r)
        expr = []
        while True:
            try:
                token, r = next(tail)
            except StopIteration:
                raise SyntaxError('unmatched "("') from None

            reader = None if r is None else readtable[r][1]
            if reader == Reader.Close:
                # TODO handle error
                assert paren.pop() == r - 1
                return None if len(expr) == 0 else expr

            expr.append(parse_tail((token, r), tail, readtable, paren))

    elif callable(reader):
        return reader((token, r), tail, readtable)

    else:
        raise SyntaxError('unmatched ")"')

def parse(tokens, readtable):
    try:
        return parse_tail(next(tokens), tokens, readtable, [])
    except StopIteration:
        return None

def expand(ast, macros):
    if type(ast) is list and len(ast) > 0:
        if type(ast[0]) is not list and ast[0] in macros:
            return expand(macros[ast[0]](*ast[1:]), macros)
        else:
            return [expand(x, macros) for x in ast]
    else:
        return ast

def read(string, readtable, macros):
    return expand(parse(lex(string, readtable), readtable), macros)

# def show(expr):
#     if type(expr) is list:
#         return '({})'.format(' '.join(map(show, expr)))
#     else:
#         return str(expr)
