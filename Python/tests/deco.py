decorated = {"simple": [], "param": []}
# simple example


def simple(func):
    decorated["simple"].append(func)

    def wrapsim(*argv):
        print(f"simple:\t{func.__name__}")
        return func(*argv)
    return wrapsim

# parameterised example


def param(myval=None):
    if myval:
        print(f"Param: {myval}")

    def decorator(func):  # true decorator
        decorated["param"].append(func)
        name = func.__name__

        def wrapparam(*argv):  # inner to return
            arg_str = ", ".join(repr(arg) for arg in argv)
            result = func(*argv)
            print(f"{name}({arg_str}):\t{result:r}")
            return result
        return wrapparam
    return decorator


@param()
def add(*argv):
    return sum(argv)


@param(22)
def sub(x, y):
    return x - y

# stacked decorators, NOTE no commutativity
@param()
@simple
def mul(*argv):
    product = 1
    for arg in argv:
        product *= arg
    return product


# main
add(1, 2, 3, 4, 5)  # equivalent to deco()(add)
sub(1, 2)  # equivalent to param(22)(sub)
mul(2, 4, 6, 8)
print(f"mul:\t{mul(1, 2, 3)}")  # 6 expected
print(decorated)
