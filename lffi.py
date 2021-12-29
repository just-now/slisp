def lffi(fun: str, params: list):
    # print(f"{fun=}, {params=}")
    if fun == "print":
        print(params[0].format(*params[1:]))
    else:
        raise(Exception(f"{fun=} not found"))
    return 0
