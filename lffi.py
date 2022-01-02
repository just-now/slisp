def lffi(fun: str, params: list):
    # print(f"{fun=}, {params=}")
    if fun == "print":
        print(params[0].format(*params[1:]))
    elif fun == "str[]":
        print(params)
        assert(len(params) == 2 and isinstance(params[1], str))
        return params[1][params[0]]
    else:
        raise(Exception(f"{fun=} not found"))
    return 0
