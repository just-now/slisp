from collections import namedtuple as tp

List = tp("List", ["seq"])                      # NOQA
Sym  = tp("Sym",  "val")                        # NOQA
Str  = tp("Str",  "val")                        # NOQA
Int  = tp("Int",  "val")                        # NOQA
Spc  = tp("Spc",  "val", defaults=(None,))      # NOQA


def atom(s: str, T, strcls) -> (str, str):  # sns, rest
    i = 0
    while i < len(s) and strcls(s[i]):
        i += 1
    return T(s[:i]), s[i:]


def sym(s: str): return atom(s, Sym, str.isalpha)
def num(s: str): return atom(s, Int, str.isnumeric)
def spc(s: str): return atom(s, Spc, str.isspace)


def sexp(s: str):
    if not s:
        return [], ''
    if s[0].isalpha():
        return sym(s)
    if s[0].isnumeric():
        return num(s)
    if s[0].isspace():
        return spc(s)
    if s[0] == ')':
        return [], s[1:]
    if s[0] == '(':
        re = s[1:]
        seq = []
        while True:
            x, re = sexp(re)
            if not x:
                break
            elif isinstance(x, Spc):
                continue
            else:
                seq += [x]
        return List(seq), re
