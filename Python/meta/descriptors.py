#!/usr/bin/env python3


class Descriptor:
    """Parent class providing default '.' handling"""

    def __init__(self, name=None):
        self.name = name

    def __set__(self, inst, value):
        inst.__dict__[self.name] = value

    def __delete__(self, inst):
        del inst.__dict__[self.name]


class Typed(Descriptor):
    """Type checking descriptor"""
    ty = object

    def __set__(self, inst, value):
        if not isinstance(value, self.ty):
            raise TypeError(f"Expected {self.ty}")
        else:
            super().__set__(inst, value)


class Integer(Typed):
    ty = int


class Positive(Descriptor):
    """Positive domain checking on numerics, descriptor"""

    def __set__(self, inst, value):
        if value < 0:
            raise ValueError("Must be <= 0")
        super().__set__(inst, value)


"""
'super()' chain goes:
    Float -> Positive -> Descriptor
to collapse diamond problem
"""


class PositiveInteger(Integer, Positive):
    pass


class Stock:
    profit = PositiveInteger('profit')


if __name__ == "__main__":
    s = Stock()
    s.profit = 10  # type checked
    print(s.profit)
    s.profit = -10
