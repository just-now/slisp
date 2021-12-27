from sys import exit
from ast import intrp, heap, Seq, Assign, While, Op, Var, Const
from lex import sexp


def main() -> int:
    s = Seq([Assign("b", Const(0)),
             Assign("a", Const(10)),
             While(Var("a"),
                   Seq([Assign("a", Op('-', Var("a"), Const(1))),
                        Assign("b", Op('+', Var("b"), Const(2)))]))])
    print(s)
    intrp(s)
    print(heap)

    # s = "(a 1 2 (b 3 4 (c (d 1) 5)))"
    s = """(
              (let b 0)
              (let a 10)

              (while a
                ((sub a 1)
                 (add b 2))))"""
    print(sexp(s))
    return 0


if __name__ == '__main__':
    exit(main())
