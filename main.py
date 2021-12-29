from sys import exit, stdin
from last import ast, intrp
from llex import sexp


def main() -> int:
    st, _ = sexp("".join(stdin.readlines()))
    intrp(ast(st))
    return 0


if __name__ == '__main__':
    exit(main())
