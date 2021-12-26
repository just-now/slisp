from collections import namedtuple as tp

Const  = tp("Var", "v")                         # NOQA
Var    = tp("Var", "v")                         # NOQA
Op     = tp("Op", ["op", "left", "right"])      # NOQA
Assign = tp("Assign", ["left", "right"])        # NOQA
While  = tp("While", ["cond", "body"])          # NOQA
If     = tp("If", ["cond", "if_", "else_"])     # NOQA
Seq    = tp("Seq", "seq")                       # NOQA
Skip   = tp("Skip", "unused", defaults=(None,)) # NOQA

heap = {}


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


s = Seq([Assign("b", Const(0)),
         Assign("a", Const(10)),
         While(Var("a"),
               Seq([Assign("a", Op('-', Var("a"), Const(1))),
                    Assign("b", Op('+', Var("b"), Const(2)))]))])
print(s)
intrp(s)
print(heap)
