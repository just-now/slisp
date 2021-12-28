def lispffi(fun: str, params: list):
    # print(f"{fun=}, {params=}")
    if fun == "print":
        print(params[0].format(*params[1:]))
    return 0
