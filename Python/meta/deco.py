#!/usr/bin/env python3
from functools import wraps
decorated = {"simple": [], "param": []}


def simple(func):
    """Simple decorator example"""
    decorated["simple"].append(func)

    @wraps(func)  # Propagates function metadata
    def wrapsim(*argv):
        print(f"simple:\t{func.__name__}")
        return func(*argv)
    return wrapsim


def param(myval=None):
    """Parameterised decorator example"""
    if myval:
        print(f"Param: {myval}")

    def decorator(func):  # real decorator
        decorated["param"].append(func)
        name = func.__name__

        @wraps(func)
        def wrapparam(*argv):  # inner to return
            arg_str = ", ".join(repr(arg) for arg in argv)
            result = func(*argv)
            print(f"{name}({arg_str}):\t{result:d}")
            return result
        return wrapparam
    return decorator


@param()  # param()(add)
def add(*argv):
    return sum(argv)


@param(22)  # param(22)(add)
def sub(x, y):
    return x - y

# stacked decorators, NOTE NOT commutative
@param()
@simple
def mul(*argv):
    product = 1
    for arg in argv:
        product *= arg
    return product


if __name__ == "__main__":
    add(1, 2, 3, 4, 5)
    sub(1, 2)
    mul(2, 4, 6, 8)
    print(f"mul:\t{mul(1, 2, 3)}")
    print({k: [x.__name__ for x in v]
           for k, v in decorated.items()})
