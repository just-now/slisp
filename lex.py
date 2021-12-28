from itertools import dropwhile as dw, takewhile as tw
from collections import namedtuple as tp

List = tp("List", ["seq"])
Atom = tp("Atom", "val")


def isatom(c: str): return not (str.isspace(c) or c in "()")


def sexp(s: str):
    if not s:
        return [], ''
    if s[0].isspace():
        return sexp("".join(dw(str.isspace, s)))
    if s[0] == ')':
        return [], s[1:]
    if s[0] == '(':
        seq = []
        re = s[1:]
        x, re = sexp(re)
        while x:
            seq += [x]
            x, re = sexp(re)
        return List(seq), re
    return Atom("".join(tw(isatom, s))), "".join(dw(isatom, s))
