from collections import namedtuple as tp
from functools import reduce
from llex import Atom, List
from lffi import lffi


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

struct = {}                                     # NOQA
heap   = {"nil": None, "false": False, "true": True}
ops    = {op: eval(f"lambda a,b: a {op} b") for op in
          "+.-.*./.==.>.<.>=.<=.and.or".split('.')}


def intrp(exp, stk=None, clo=None):
    match exp:                                  # NOQA
        case Skip():
            return
        case Const(c):
            return c
        case Var(("#", _)) as lfun:
            return lfun
        case Var(v):
            if stk and (v in stk):
                return stk[v]
            return clo[v] if (clo and (v in clo)) else heap[v]
        case Op(op, l, r):
            left, right = intrp(l, stk, clo), None
            if not op in ["and", "or"] or \
               ((op == "and" and left) or (op == "or" and not left)):
                right = intrp(r, stk, clo)
            return ops[op](left, right)
        case Assign(l, r):
            # out = stk if stk else heap
            out = heap if l in heap else (stk if stk else heap)
            out[l] = intrp(r, stk, clo)
            return
        case Seq(seq):
            assert(isinstance(seq, list))
            for q in seq:
                ret = intrp(q, stk, clo)
            return ret
        case If(cond, if_, else_):
            if_else = if_ if intrp(cond, stk, clo) else else_
            return intrp(if_else, stk, clo)
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
            afunps, lfunps = [], intrp(params[1], stk, clo)
            while lfunps:
                afunps += [Const(lfunps["params"]["data"])]
                lfunps = lfunps["params"]["next"]
            return intrp(Apply(fun, afunps), stk, clo)
        case Apply(fun, params):
            foo, fun = (heap.get(fun), fun) if isinstance(fun, str) else (fun, "")
            scons, *saccr = fun.split('.')
            structure = struct.get(scons)

            if foo and foo.get("&rest"):
                pn = len(foo["params"])-1
                rest, params, nil = reversed(params[pn:]), params[:pn], Var("nil")
                params += [reduce(lambda p, ps: Apply('List', [ps, p]), rest, nil)]

            iparams = [intrp(p, stk, clo) for p in params]
            if saccr:      # accessors have 1 parameter only
                assert(len(saccr) in [1, 2])
                assert(len(params) == len(iparams) == len(saccr))
                if len(saccr) == 2:
                    assert(saccr[1] == "set" and
                           saccr[0] in iparams[0]["params"] and
                           iparams[0]["struct"] == scons)
                    iparams[0]["params"][saccr[0]] = iparams[1]
                    return iparams[0]
                else:
                    return iparams[0]["params"][saccr[0]]
            if structure:  # construction of the structure
                assert(len(params) == len(structure))
                return {"struct": fun, "params": dict(zip(structure, iparams))}
            if foo:
                assert(len(params) == len(foo["params"]))
                newstack = dict(zip(foo["params"], iparams))
                newclo = foo.get("closure")
                return intrp(foo["body"], newstack, newclo)
            return lffi(fun, iparams)
        case Fun(fun, params, body):
            params = [p.v for p in params]
            rcount = params.count("&rest")
            assert(rcount in [0, 1] and (rcount != 1 or params[-2] == "&rest"))
            heap[fun.v] = {"params": [p for p in params if p != "&rest"],
                           "body": body, "&rest": rcount == 1,
                           "closure": stk if fun.v == "lambda" else None}
            return Var((f"#{fun.v}", heap[fun.v]))
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
        case List([Atom("if"), cond, texp]):
            return If(ast(cond), ast(texp), Skip())
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
                return Const(float(v)) if '.' in v else Const(int(v))
        case _:
            return e
