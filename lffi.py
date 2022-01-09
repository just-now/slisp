import _io
import sys
from functools import reduce


ffit = {
    "format": str.format,
    "file-open": _io.open,
    "file-close": _io.TextIOWrapper.close,
    "file-read":  _io.TextIOWrapper.read,
    "file-write":  _io.TextIOWrapper.write,
    "file-readline": _io.TextIOWrapper.readline,
    "str-join": lambda sep, ls: sep.join(from_list(ls)),
    "str-split": lambda *s: to_list(str.split(*s)),
    "str": str, "int": int, "float": float,
    "err?": lambda p: isinstance(p, int),
    "to-list": lambda l: to_list(l),
}


def check_list_params(ps, ps_nr: int) -> bool:
    return len(ps) == ps_nr and all([isinstance(p, dict) and
                                     p["struct"] == "List" for p in ps])


def to_list(whatever: list):
    return reduce(lambda n, d: {"struct": "List", "params":
                                {"data": d, "next": n}},
                  reversed(whatever), None)


def from_list(slist) -> list:
    assert(check_list_params([slist], 1))
    ret = []
    while slist:
        ret += [slist["params"]["data"]]
        slist = slist["params"]["next"]
    return ret


def lffi(fun: str, params: list):
    try:
        if fun == "print":
            return print(params[0].format(*params[1:]), end="")
        elif fun == "append":
            assert(check_list_params(params, 2))
            return to_list(from_list(params[0]) + from_list(params[1]))
        elif fun in ffit:
            return ffit[fun](*params)
    except OSError as e:
        return e.args[0]
    except Exception as e:
        print(e.with_traceback(), file=sys.stderr)
        return -1

    raise(Exception(f"{fun=} not found"))
