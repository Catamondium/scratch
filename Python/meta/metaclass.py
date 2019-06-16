#!/usr/bin/env python3


def makeHook(func):
    """Special method marker decorator"""
    func.is_hook = True
    return func


class MetaHook(type):
    """
    Metaclass intercepting class creation,
    forms __special__ from marked methods
    """
    def __new__(cls, name, bases, dct):
        newdct = {}
        for k, v in dct.items():
            if getattr(v, 'is_hook', False):
                newdct[f"__{k}__"] = v
            else:
                newdct[k] = v
        return super().__new__(cls, name, bases, newdct)


class Hook(metaclass=MetaHook):
    """MetaHook convienience wrapper"""
    pass


class Vec(Hook):  # inherits metaclass=MetaHook
    """Concrete hooked Vector(x, y) class"""

    def __init__(self, x=0, y=0):
        self.x = x
        self.y = y

    @makeHook
    def add(self, other):
        return Vec(self.x + other.x, self.y + other.y)

    @makeHook
    def repr(self):  # trasformed to __repr__
        return f"Vec({self.x}, {self.y})"


if __name__ == "__main__":
    vertex1, vertex2 = Vec(0, 1), Vec(1, 0)
    print(vertex1 + vertex2)
