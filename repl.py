#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import readline, sexpr, lc

def reader_id(token, tokens, readtable):
    return token[0]

def reader_integer(token, tokens, readtable):
    return int(token[0])

if __name__ == '__main__':
    readtable = [
        (r'\(', sexpr.Reader.Open), (r'\)', sexpr.Reader.Close),
        (r'\[', sexpr.Reader.Open), (r'\]', sexpr.Reader.Close),

        (r'[a-zA-Z][a-zA-Z_0-9]*', reader_id),
        (r'(-+)?[0-9]+', reader_integer),
    ]

    env = {
        'natify': lambda e: lc.Apply(e, lambda n: n + 1, 0)
    }

    def lookup(e):
        if type(e) is str:
            return env[e]
        elif type(e) is lc.Apply:
            e[0] = lookup(e[0])
            e[1] = lookup(e[1])
        elif type(e) is lc.Lambda:
            e.e = lookup(e.e)
        return e

    def eval(e):
        return lc.normalorder(lookup(lc.parse(e)))

    while True:
        try:
            string = input('> ')
        except KeyboardInterrupt:
            exit()

        ast = sexpr.parse(sexpr.lex(string, readtable), readtable)

        if type(ast) is list and ast[0] == 'set':
            assert len(ast) == 3 and type(ast[1]) is str
            env[ast[1]] = eval(ast[2])

        elif type(ast) is list and ast[0] == 'save':
            assert len(ast) == 3
            with open(ast[1]) as f:
                s = eval(ast[2])
                f.write(s)

        else:
            exp = eval(ast)
            if exp is not None:
                print(exp)
                env['_'] = exp
