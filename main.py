from sys import exit, stdin
from ast import ast, intrp, heap
from lex import sexp


def main() -> int:
    st, _ = sexp("".join(stdin.readlines()))
    intrp(ast(st))
    print(heap)
    return 0


if __name__ == '__main__':
    exit(main())
