def lffi(fun: str, params: list):
    # print(f"{fun=}, {params=}")
    if fun == "print":
        print(params[0].format(*params[1:]), end="")
    elif fun == "str[]":
        assert(len(params) == 2 and isinstance(params[1], str))
        return params[1][params[0]]
    elif fun == "append":
        assert(len(params) == 2)
        assert(isinstance(params[0], dict) and
               isinstance(params[1], dict) and
               params[0]["struct"] == "List" and
               params[1]["struct"] == "List")
        x = params[0]
        while x["params"]["next"]:
            x = x["params"]["next"]
        x["params"]["next"] = params[1]
        return params[0]

    else:
        raise(Exception(f"{fun=} not found"))
    return 0
