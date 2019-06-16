#!/usr/bin/env python3
from functools import wraps


def keyparams(**kwargs):
    """callDebug helper, generating kwargs signature"""
    return f"{', ' if len(kwargs) != 0 else ''}{', '.join([f'{k}={v}' for k,v in kwargs.items()])}"


def callDebug(func):
    """Callsite debugging decorator"""
    @wraps(func)
    def thing(*args, **kwargs):
        name = func.__qualname__
        result = func(*args, **kwargs)
        print(
            f"""\
{name}({', '.join([repr(x) for x in args])}{keyparams(**kwargs)}\
{f' -> {result}' if result else ''}""")
    return thing


def excempt(func):
    """Make method excempt from callDebugClass propagation"""
    func.noDeco = True
    return func


def callDebugClass(cls):
    """
    Decorate all callable attributes with callDebug,
    unlike metaclassing, decoration occurs after class creation.

    Main distinguishing point, the metaclass is capable of modifying __dict__,
    the decorator may not.
    """
    for k, v in cls.__dict__.items():
        if callable(v) and not getattr(v, 'noDeco', False) and k != '__init__':
            setattr(cls, k, callDebug(v))  # propagate callDebug
    return cls


@callDebugClass
class Vec:
    def __init__(self, x=0, y=0):
        self.x = x
        self.y = y

    def __add__(self, other):  # Has callDebug applied
        return Vec(self.x + other.x, self.y + other.y)

    def __abs__(self):
        return (self.x**2 + self.y**2)**.5

    @excempt
    def __repr__(self):
        return f"Vec({self.x}, {self.y})"


@callDebug
def add(x, y, key=None):
    """Simple usage"""
    print(f"Key: {key}")
    return x + y


if __name__ == "__main__":
    add(2, 4, key="ABC")
    vertex1, vertex2 = Vec(1, 0), Vec(0, 1)
    print(vertex1 + vertex2)
    print(abs(Vec(3, 4)))
