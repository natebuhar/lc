# -*- coding: utf-8 -*-

import sexpr

class Var:
    def __init__(self, v):
        self.v = v

    def __eq__(self, other):
        return self.v == other

    def __str__(self):
        return '#' + str(self.v)

class Lambda:
    def __init__(self, e):
        self.e = e

    def __eq__(self, other):
        if isinstance(other, Lambda):
            return self.e == other.e
        return False

    def __str__(self):
        return 'λ {}'.format(self.e)

class Apply:
    def __init__(self, *args):
        if len(args) > 2:
            self.e = [Apply(*args[:-1]), args[-1]]
        else:
            assert len(args) > 1
            self.e = list(args)

    def __getitem__(self, index):
        return self.e[index]

    def __setitem__(self, index, value):
        self.e[index] = value

    def __eq__(self, other):
        if isinstance(other, Apply):
            return all(x == y for x, y in zip(self, other))
        return False

    def __str__(self):
        return '({})'.format(' '.join(map(str, self.e)))

def shift(e, i, d):
    """ Increment/decrement all free variables in e by i """
    if type(e) is Var and e.v >= d:
        return Var(e.v + i)
    elif isinstance(e, Apply):
        return Apply(shift(e[0], i, d), shift(e[1], i, d))
    elif isinstance(e, Lambda):
        return Lambda(shift(e.e, i, d + 1))
    else:
        return e

def free(e, v):
    """ Is v a free variable in e? """
    if type(e) is Var:
        return e == v
    elif isinstance(e, Apply):
        return free(e[0], v) or free(e[1], v)
    elif isinstance(e, Lambda):
        return free(e.e, v + 1)
    else:
        return e

def subs(e, v, n):
    """ Substitute n for the variable v in e: e[v := n] """
    if type(e) is Var:
        return n if e == v else e
    elif isinstance(e, Apply):
        return Apply(subs(e[0], v, n), subs(e[1], v, n))
    elif isinstance(e, Lambda):
        # increment v and all free variables in n to avoid capture by e
        return Lambda(subs(e.e, v + 1, shift(n, 1, 0)))
    else:
        return e

def isbeta(e):
    """ Is e a beta-redex? """
    return \
        isinstance(e, Apply) and \
        isinstance(e[0], Lambda)

def iseta(e):
    """ Is e an eta-redex? """
    return \
        isinstance(e, Lambda)  and \
        isinstance(e.e, Apply) and \
        e.e[1] == 0            and \
        not free(e.e[0], 0)

def beta(e):
    """ Beta-reduce e: 'x.e n = e[x := n]' """
    # increment all free variables in e[1] to avoid capture by e[0],
    a = shift(e[1], 1, 0)
    b = subs(e[0].e, 0, a)

    # we unwrapped one level of abstract, so decrement all free variables
    return shift(b, -1, 0)

def eta(e):
    """ Eta-convert e: 'x.f x = f' """
    return shift(e.e[0], -1, 0)

def fullbeta(e):
    if isinstance(e, Apply):
        r = Apply(fullbeta(e[0]), fullbeta(e[1]))
        return fullbeta(beta(r)) if isbeta(r) else r
    elif isinstance(e, Lambda):
        r = Lambda(fullbeta(e.e))
        return r
    else:
        return e

def normalorder(e):
    if isinstance(e, Apply):
        r = Apply(normalorder(e[0]), e[1])
        if callable(e[0]):
            f, x = e
            return normalorder(f(normalorder(x)))
        else:
            return normalorder(beta(r)) if isbeta(r) else r
    else:
        return e

def macro_lambda(params, body):
    if params is None:
        return ['λ', '_', body]
    elif len(params) == 1:
        return ['λ', params[0], body]
    else:
        return ['λ', params[0], ['lambda', params[1:], body]]

def macro_let(bindings, body):
    if bindings is None:
        return body
    elif len(bindings) == 1:
        n, v = bindings[0]
        return [['λ', n, body], v]
    else:
        n, v = bindings[0]
        return [['λ', n, ['let', bindings[1:], body]], v]

macros = {'lambda': macro_lambda, 'let': macro_let}

def parse(e, bindings=[], macros=macros):
    def f(e, bindings):
        if type(e) is list:
            if e[0] in ('λ', '\\'):
                assert len(e) == 3 and type(e[1]) is str
                return Lambda(f(e[2], [e[1]] + bindings))
            elif len(e) == 1:
                return f(e[0], bindings)
            elif None in e:
                return f([x for x in e if x is not None], bindings)
            else:
                return Apply(*[f(x, bindings) for x in e])
        else:
            return Var(bindings.index(e)) if e in bindings else e

    return f(sexpr.expand(e, macros), bindings)

def read(string):
    readtable = [
        (r"\(", sexpr.Reader.Open), (r"\)", sexpr.Reader.Close),
        (r"\[", sexpr.Reader.Open), (r"\]", sexpr.Reader.Close),
    ]
    e = sexpr.parse(sexpr.lex(string, readtable), readtable)
    return parse(e, macros=macros)

def test_1():
    n = fullbeta
    l = Lambda
    a = Apply

    Z     = l(l(0))
    SZ    = l(l(a(1, 0)))
    SSZ   = l(l(a(1, a(1, 0))))
    SSSZ  = l(l(a(1, a(1, a(1, 0)))))
    SSSSZ = l(l(a(1, a(1, a(1, a(1, 0))))))
    S     = l(l(l(a(1, a(2, 1, 0)))))
    PLUS  = l(l(l(l(a(3, 1, a(2, 1, 0))))))
    PRED  = l(l(l(a(2, l(l(a(0, a(1, 3)))), l(1), l(0)))))
    SUB   = l(l(a(0, PRED, 1)))

    assert n(Z) == n(Z)
    assert n(a(S, Z)) == n(SZ)
    assert n(a(S, a(S, Z))) == n(SSZ)
    assert n(a(S, a(S, a(S, Z)))) == n(SSSZ)
    assert n(a(S, a(S, a(S, a(S, Z))))) == n(SSSSZ)

    assert n(a(PLUS, SZ, SZ)) == n(SSZ)
    assert n(a(PLUS, SSZ, SZ)) == n(SSSZ)
    assert n(a(PLUS, SZ, SSZ)) == n(SSSZ)
    assert n(a(PLUS, SZ, SSSZ)) == n(SSSSZ)
    assert n(a(PLUS, SSSZ, SZ)) == n(SSSSZ)

    assert n(a(SUB, SSSZ, SSZ)) == n(SZ)
    assert n(a(SUB, SSSSZ, SSZ)) == n(SSZ)
    assert n(a(SUB, SSZ, SSSSZ)) == n(Z)
    assert n(a(SUB, SSZ, SSZ)) == n(Z)

def test_2():
    def natify(e):
        return Apply(e, lambda n: n + 1, 0)

    import sys
    with open(sys.argv[1]) as f:
        ast = read(f.read())

    print(ast)
    print(normalorder(natify(ast)))

def test_3():
    def haskstr(e):
        if type(e) is Var:
            return '(V {})'.format(e.v)
        elif type(e) is Apply:
            return '(A {})'.format(' '.join(map(haskstr, e)))
        elif type(e) is Lambda:
            return '(L {})'.format(haskstr(e.e))

    import sys
    with open(sys.argv[1]) as f:
        ast = read(f.read())

    print(haskstr(ast))
