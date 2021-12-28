from collections import namedtuple as tp
from lex import Atom, List, sexp
from lispffi import lispffi


Const  = tp("Const", "c")                       # NOQA
Var    = tp("Var", "v")                         # NOQA
Op     = tp("Op", ["op", "left", "right"])      # NOQA
Assign = tp("Assign", ["left", "right"])        # NOQA
While  = tp("While", ["cond", "body"])          # NOQA
If     = tp("If", ["cond", "if_", "else_"])     # NOQA
Seq    = tp("Seq", "seq")                       # NOQA
Apply  = tp("Apply", ["fun", "params"])         # NOQA
Fun    = tp("Fun", ["fun", "params", "body"])   # NOQA
Skip   = tp("Skip", "unused", defaults=(None,)) # NOQA
heap   = {}                                     # NOQA
foos   = {}                                     # NOQA


def intrp(exp, stk=None):
    match exp:                                  # NOQA
        case Const(c):
            return c
        case Var(v):
            return stk[v] if stk and (v in stk) else heap[v]
        case Op(op, l, r):
            match op:
                case '+': return intrp(l, stk) + intrp(r, stk)
                case '-': return intrp(l, stk) - intrp(r, stk)
        case Assign(l, r):
            heap[l] = intrp(r, stk)
            return
        case Seq(seq):
            assert(isinstance(seq, list))
            for q in seq:
                intrp(q, stk)
            return
        case If(cond, if_, else_):
            intrp(if_, stk) if intrp(cond, stk) else intrp(else_, stk)
            return
        case While(cond, body):
            while intrp(cond, stk) != 0:
                intrp(body, stk)
            return
        case Apply(fun, params):
            if foos.get(fun):
                assert(len(params) == len(foos[fun]["params"]))
                newstack = dict(zip(foos[fun]["params"],
                                    [intrp(p, stk) for p in params]))
                return intrp(foos[fun]["body"], newstack)
            else:
                return lispffi(fun, [intrp(p, stk) for p in params])
        case Fun(fun, params, body):
            foos[fun.v] = {"params": [p.v for p in params], "body": body}
            return


def ast(e):
    match e:
        case List([List(l1), *l2] as seq):
            return Seq([ast(s) for s in seq])
        case List([Atom("defun"), fun, List(params), body]):
            return Fun(ast(fun), [ast(p) for p in params], ast(body))
        case List([Atom("setq"), Atom(v), exp]):
            return Assign(v, ast(exp))
        case List([Atom("while"), cond, exp]):
            return While(ast(cond), ast(exp))
        case List([Atom("if"), cond, texp, fexp]):
            return If(ast(cond), ast(texp), ast(fexp))
        case List([Atom("+" | "-" as op), left, right]):
            return Op(op, ast(left), ast(right))
        case List([Atom(fun), *params]):
            return Apply(fun, [ast(p) for p in params])
        case Atom(v):
            if str.isalpha(v):
                return Var(v)
            elif v[0] == v[-1] == '"':  # strings
                return Const(v[1:-1]
                             .replace('\\n', '\n')  # [sigh] clean string
                             .replace('\\t', '\t')
                             .replace('\\"', '"'))
            else:
                return Const(int(v))
        case _:
            return e
