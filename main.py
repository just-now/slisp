from sys import exit, stdin
from yst import ast, intrp
from lex import sexp


def main() -> int:
    st, _ = sexp("".join(stdin.readlines()))
    intrp(ast(st))
    return 0


if __name__ == '__main__':
    exit(main())
