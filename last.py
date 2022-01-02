from collections import namedtuple as tp
from llex import Atom, List, sexp
from lffi import lffi
from functools import reduce


Const  = tp("Const", "c")                       # NOQA
Var    = tp("Var", "v")                         # NOQA
Op     = tp("Op", ["op", "left", "right"])      # NOQA
Assign = tp("Assign", ["left", "right"])        # NOQA
While  = tp("While", ["cond", "body"])          # NOQA
If     = tp("If", ["cond", "if_", "else_"])     # NOQA
Seq    = tp("Seq", "seq")                       # NOQA
Apply  = tp("Apply", ["fun", "params"])         # NOQA
Fun    = tp("Fun", ["fun", "params", "body"])   # NOQA
Struct = tp("Struct", ["name", "params"])       # NOQA
Skip   = tp("Skip", "unused", defaults=(None,)) # NOQA


heap   = {"nil": None}                          # NOQA
foos   = {}                                     # NOQA
struct = {}                                     # NOQA


def intrp(exp, stk=None):
    match exp:                                  # NOQA
        case Const(c):
            return c
        case Var(v):
            return stk[v] if stk and (v in stk) else heap[v]
        case Op(op, l, r):
            match op:
                case "+": return intrp(l, stk) + intrp(r, stk)
                case "-": return intrp(l, stk) - intrp(r, stk)
                case "==": return intrp(l, stk) == intrp(r, stk)
                case ">": return intrp(l, stk) > intrp(r, stk)
                case "<": return intrp(l, stk) < intrp(r, stk)
                case ">=": return intrp(l, stk) >= intrp(r, stk)
                case "<=": return intrp(l, stk) <= intrp(r, stk)
                case "and": return intrp(l, stk) and intrp(r, stk)
                case "or": return intrp(l, stk) or intrp(r, stk)
        case Assign(l, r):
            out = stk if stk else heap
            out[l] = intrp(r, stk)
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
            while bool(intrp(cond, stk)):
                intrp(body, stk)
            return
        case Apply(fun, params):
            cons, *acc = fun.split('.')
            stp = struct.get(cons)
            foo = foos.get(fun)
            spa = [intrp(p, stk) for p in params]
            if stp and acc:  # accessors have 1 parameter only
                assert(len(params) == len(spa) == 1)
                return spa[0]["params"][acc[0]]
            if stp:  # construction of the structure
                assert(len(params) == len(stp))
                return {"struct": fun, "params": dict(zip(stp, spa))}
            if foo:
                if foo["&rest"]:
                    pn = len(foo["params"])-1
                    rest, params = params[pn:], params[:pn]
                    params += [reduce(lambda x, y: Apply('List', [y, x]),
                                      reversed(rest), Var("nil"))]
                    spa = [intrp(p, stk) for p in params]

                assert(len(params) == len(foo["params"]))
                newstack = dict(zip(foo["params"], spa))
                return intrp(foo["body"], newstack)
            return lffi(fun, spa)
        case Fun(fun, params, body):
            params = [p.v for p in params]
            rcount = params.count("&rest")
            assert(rcount in [0, 1] and (rcount != 1 or params[-2] == "&rest"))
            foos[fun.v] = {"params": [p for p in params if p != "&rest"],
                           "body": body, "&rest": rcount == 1}
            return
        case Struct(name, params) as sx:
            struct[name.v] = [m.v for m in params]
            return


def ast(e):
    match e:
        case List([List(l1), *l2] as seq):
            return Seq([ast(s) for s in seq])
        case List([Atom("defstruct"), name, *params]):
            return Struct(ast(name), [ast(m) for m in params])
        case List([Atom("defun"), fun, List(params), body]):
            return Fun(ast(fun), [ast(p) for p in params], ast(body))
        case List([Atom("setq"), Atom(v), exp]):
            return Assign(v, ast(exp))
        case List([Atom("while"), cond, exp]):
            return While(ast(cond), ast(exp))
        case List([Atom("if"), cond, texp, fexp]):
            return If(ast(cond), ast(texp), ast(fexp))
        case List([Atom("+" | "-" | "==" | ">" | "<" | "<=" | ">="
                        | "and" | "or" as op), left, right]):
            return Op(op, ast(left), ast(right))
        case List([Atom(fun), *params]):
            return Apply(fun, [ast(p) for p in params])
        case Atom(v):
            if str.isalpha(v[0]) or v[0] in "&#":
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
