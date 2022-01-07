def lffi(fun: str, params: list):
    # print(f"{fun=}, {params=}")
    if fun == "print":
        print(params[0].format(*params[1:]), end="")
    elif fun == "format":
        return params[0].format(*params[1:])
    elif fun == "open":
        assert(len(params) == 2)
        return open(params[0], params[1])
    elif fun == "close":
        assert(len(params) == 1)
        fd = params[0]
        fd.close()
        return
    elif fun == "read":
        assert(len(params) == 2)
        fd = params[0]
        sz = params[1]
        return fd.read(sz)
    elif fun == "write":
        assert(len(params) == 2)
        fd = params[0]
        text = params[1]
        return fd.write(text)
    elif fun == "readline":
        assert(len(params) == 2)
        fd = params[0]
        sz = params[1]
        return fd.readline(sz)
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
