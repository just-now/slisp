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


heap   = {"nil": None, "false": False, "true": True}
struct = {}
ops    = {op: eval(f"lambda a,b: a {op} b") for op in
          "+.-.*./.==.>.<.>=.<=.and.or".split('.')}


def intrp(exp, stk=None, clo=None):
    match exp:                                  # NOQA
        case Const(c):
            return c
        case Var(("#", _)) as lfun:
            return lfun
        case Var(v):
            if stk and (v in stk):
                return stk[v]
            elif clo and (v in clo):
                return clo[v]
            return heap[v]
        case Op(op, l, r):
            return ops[op](intrp(l, stk, clo), intrp(r, stk, clo))
        case Assign(l, r):
            out = stk if stk else heap
            out[l] = intrp(r, stk, clo)
            return
        case Seq(seq):
            assert(isinstance(seq, list))
            ret = None
            for q in seq:
                ret = intrp(q, stk, clo)
            return ret
        case If(cond, if_, else_):
            return intrp(if_, stk, clo) if intrp(cond, stk, clo) \
                else intrp(else_, stk, clo)
        case While(cond, body):
            while bool(intrp(cond, stk, clo)):
                intrp(body, stk, clo)
            return
        case Apply("funcall", params):
            _, fun = intrp(params[0], stk, clo).v
            return intrp(Apply(fun, [p for p in params[1:]]), stk, clo)
        case Apply("apply", params):
            assert(len(params) == 2)
            _, fun = intrp(params[0], stk, clo).v
            lfunps = intrp(params[1], stk, clo)
            afunps = []
            while lfunps:
                afunps += [Const(lfunps["params"]["data"])]
                lfunps = lfunps["params"]["next"]
            return intrp(Apply(fun, afunps), stk, clo)
        case Apply(fun, params):
            if isinstance(fun, str):
                foo = heap.get(fun)
            else:  # lambda
                foo, fun = fun, ""
            cons, *acc = fun.split('.')
            stp = struct.get(cons)
            spa = [intrp(p, stk, clo) for p in params]
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
                    spa = [intrp(p, stk, clo) for p in params]

                assert(len(params) == len(foo["params"]))
                newstack = dict(zip(foo["params"], spa))
                newclo = foo.get("closure")
                return intrp(foo["body"], newstack, newclo)
            return lffi(fun, spa)
        case Fun(fun, params, body):
            params = [p.v for p in params]
            rcount = params.count("&rest")
            assert(rcount in [0, 1] and (rcount != 1 or params[-2] == "&rest"))
            ret = {"params": [p for p in params if p != "&rest"],
                   "body": body, "&rest": rcount == 1}
            if fun.v != "lambda":
                heap[fun.v] = ret
            else:
                ret["closure"] = stk
                return Var(("#lambda", ret))
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
        case List([Atom("lambda"), List(params), body]):
            return Fun(Var("lambda"), [ast(p) for p in params], ast(body))
        case List([Atom("setq"), Atom(v), exp]):
            return Assign(v, ast(exp))
        case List([Atom("while"), cond, exp]):
            return While(ast(cond), ast(exp))
        case List([Atom("if"), cond, texp, fexp]):
            return If(ast(cond), ast(texp), ast(fexp))
        case List([Atom(fun), *params]):
            aps = [ast(p) for p in params]
            is_op = fun in ops and len(aps) == 2
            return Op(fun, *aps) if is_op else Apply(fun, aps)
        case Atom(v):
            if str.isalpha(v[0]) or v[0] == "&":
                return Var(v)
            elif v[0] == "#":           # lambda, func as a parameter
                return Var(("#", v[1:]))
            elif v[0] == v[-1] == '"':  # strings
                return Const(v[1:-1])
            else:
                return Const(int(v))
        case _:
            return e
