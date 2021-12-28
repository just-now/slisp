from collections import namedtuple as tp
from lex import Atom, List, sexp


Const  = tp("Const", "c")                       # NOQA
Var    = tp("Var", "v")                         # NOQA
Op     = tp("Op", ["op", "left", "right"])      # NOQA
Assign = tp("Assign", ["left", "right"])        # NOQA
While  = tp("While", ["cond", "body"])          # NOQA
If     = tp("If", ["cond", "if_", "else_"])     # NOQA
Seq    = tp("Seq", "seq")                       # NOQA
Skip   = tp("Skip", "unused", defaults=(None,)) # NOQA
heap   = {}                                     # NOQA


def intrp(exp):
    match exp:                                  # NOQA
        case Const(c):
            return c
        case Var(v):
            return heap[v]
        case Op(op, l, r):
            match op:
                case '+': return intrp(l) + intrp(r)
                case '-': return intrp(l) - intrp(r)
        case Assign(l, r):
            heap[l] = intrp(r)
            return
        case Seq(seq):
            assert(isinstance(seq, list))
            for q in seq:
                intrp(q)
            return
        case If(cond, if_, else_):
            intrp(if_) if cond else intrp(else_)
            return
        case While(cond, body):
            while intrp(cond) != 0:
                intrp(body)
            return


def ast(e):
    match e:
        case List([List(l1), *l2] as seq):
            return Seq([ast(s) for s in seq])
        case List([Atom("let"), Atom(v), exp]):
            return Assign(v, ast(exp))
        case List([Atom("while"), cond, exp]):
            return While(ast(cond), ast(exp))
        case List([Atom("+" | "-" as op), left, right]):
            return Op(op, ast(left), ast(right))
        case Atom(v):
            if str.isalpha(v):
                return Var(v)
            else:
                return Const(int(v))
        case _:
            return e
