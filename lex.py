from collections import namedtuple as tp

List = tp("List", ["seq"])
Sym  = tp("Sym",  "val")
Str  = tp("Str",  "val")
Int  = tp("Int",  "val")
Spc  = tp("Spc",  "val", defaults=(None,))

s = "(a 1 2 (b 3 4 (c (d 1) 5)))"

#                                    sns, rest
def synnumspc(s: str, T, strcls) -> (str, str):
    i = 0
    while i < len(s) and strcls(s[i]):
        i+=1
    return T(s[:i]), s[i:]

sym = lambda s: synnumspc(s, Sym, str.isalpha)
num = lambda s: synnumspc(s, Int, str.isnumeric)
spc = lambda s: synnumspc(s, Spc, str.isspace)

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

print(sexp(s))


