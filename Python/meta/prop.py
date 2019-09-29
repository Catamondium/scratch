"""
@property testing setter
"""
from math import pi


class Circle:
    def __init__(self, radius=1.0):
        self.radius = radius

    def _getcirc(self):
        return 2 * pi * self.radius

    def _setcirc(self, ncirc):
        self.radius = ncirc / (2 * pi)

    def _getdia(self):
        return 2 * self.radius

    def _setdia(self, ndia):
        self.radius = ndia / 2

    circ = property(_getcirc, _setcirc)  # circumference
    dia = property(_getdia, _setdia)  # diameter

    def __repr__(self):
        return f"Circle({self.radius})"


if __name__ == '__main__':
    c = Circle()
    print(c)  # radius 1
    c.dia = 4  # setter adapts radius
    print(c)  # radius 2
