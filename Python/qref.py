import sys
import math


def debug(func):
    """
    Print the decorated function's running signature.
    """
    name = func.__name__

    def wrap(*args):
        argstr = ", ".join(repr(arg) for arg in args)
        result = func(*args)
        print(f"{name}({argstr}):\t{result:r}")
        return result
    return wrap


def Qlocate(seq, Qn):  # working
    """
    Find the position of quartiles Qn in seq.
    """
    pos = Qn * (len(seq) / 4)
    if (pos % 1):
        ret = [math.floor(pos), math.ceil(pos)]
    else:
        ret = [math.floor(pos)]
    return ret


def Qslice(seq, Qn, x):  # working
    """
    Get all x seq about Qn.
    """
    pos = Qlocate(seq, Qn)
    centre = sum(pos) / len(pos)
    half = x / 2
    return slice(math.floor(centre-half),
                 math.floor(centre+half))


@debug
def Quartile(seq, Qn):
    """
    Get quartiles Qn in seq.
    """
    ret = []
    pos = Qlocate(seq, Qn)
    for p in pos:
        ret.append(seq[p])
    return ret


if __name__ == "__main__":
    evenset = [x for x in range(1, 51)]  # 1 - 50, 50 numbers
    oddset = [x for x in range(0, 11)]  # 0 - 10, 11 numbers

    print(f"Sliced: {evenset[Qslice(evenset, 2, 5)]}")
