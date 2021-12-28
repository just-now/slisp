from itertools import dropwhile as dw, takewhile as tw
from collections import namedtuple as tp

List = tp("List", ["seq"])
Atom = tp("Atom", "val")
prev_atoms = []


def isatom(c: str):
    if not prev_atoms:
        if c == '"':
            prev_atoms.append(c)
        return not (str.isspace(c) or c in "()")
    else:
        if prev_atoms[-1] == '\\':
            prev_atoms.pop()
        elif c == '\\':
            prev_atoms.append(c)
        elif c == '"':
            assert(prev_atoms.pop() == c)
        return True


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
