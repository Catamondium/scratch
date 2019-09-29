"""
Quadtree impl
"""

from typing import List
import tkinter as tk

COMP = "| "


class Vec:
    def __init__(self, x: float = 0, y: float = 0):
        self.x = x
        self.y = y

    def __repr__(self):
        return f"Vec({self.x}, {self.y})"


class AABB:
    def __init__(self, x: float, y: float, w: float, h: float):
        self.x = x
        self.y = y
        self.w = w
        self.h = h

    def __contains__(self, item):
        assert (isinstance(item, Vec))
        return (
                self.x <= item.x <= self.x + self.w and
                self.y <= item.y <= self.y + self.h
        )

    def intersects(self, other):
        assert(isinstance(other, AABB))
        min1 = Vec(self.x, self.y)
        max1 = Vec(self.x + self.w, self.y + self.h)
        return (
            # Self is inside other
            min1 in other or
            max1 in other or
            (
                # Self surrounds other
                min1.x < other.x and
                min1.y < other.y and
                max1.x > other.x + other.w and
                max1.y > other.y + other.h
            )
        )

    def params(self):
        """
        Return param list
        :return: x,y,w,h tuple
        """
        return self.x, self.y, self.w, self.h


class QTree:
    def __init__(self, bounds: AABB, lim=5):
        self.lim = lim
        self.bounds = bounds
        self.points: List[Vec] = list()
        self.children: List[QTree] = list()

    def _split(self):
        half_w = self.bounds.w / 2
        half_h = self.bounds.h / 2
        size = (half_w, half_h)

        sw = AABB(self.bounds.x, self.bounds.y, *size)
        se = AABB(self.bounds.x + half_w, self.bounds.y, *size)
        nw = AABB(self.bounds.x, self.bounds.y + half_h, *size)
        ne = AABB(self.bounds.x + half_w, self.bounds.y + half_h, *size)

        self.children.append(QTree(nw, self.lim))
        self.children.append(QTree(ne, self.lim))
        self.children.append(QTree(sw, self.lim))
        self.children.append(QTree(se, self.lim))

    def insert(self, point: Vec):
        """
        Insert point into the tree
        :param point: point to insert
        :return: None
        """
        if point not in self.bounds:
            return

        if len(self.points) < self.lim:
            self.points.append(point)
        elif not self.children:
            self._split()

        for child in self.children:
            child.insert(point)

    def clear(self):
        """
        Clears tree
        :return: None
        """
        self.points.clear()
        self.children.clear()

    def query(self, box: AABB):
        if not self.bounds.intersects(box):
            return

        for p in self.points:
            if p in box:
                yield p

        for child in self.children:
            yield from child.query(box)

    def string_view(self, indent=COMP, root=True) -> str:
        """
        stringify expanded tree
        :return: visualised tree
        """

        sident = "" if root else indent
        ret = f"{sident}{'' if root else '|-'}T{self.bounds.params()}"
        if self.points:
            ret += '\n' + "\n".join(f"{sident + COMP}|-{p}" for p in self.points)

        if self.children:
            ret += '\n' + "\n".join(ch.string_view(indent + COMP, False) for ch in self.children)
        return ret

    def draw(self, canvas: tk.Canvas):
        canvas.create_rectangle(*self.bounds.params(), outline='blue')

        for p in self.points:
            x1, y1 = p.x - 1, p.y - 1
            x2, y2 = p.x + 1, p.y + 1
            canvas.create_oval(x1, y1, x2, y2, fill='red')

        for child in self.children:
            child.draw(canvas)

    def items(self):
        for p in self.points:
            yield p

        for ch in self.children:
            yield from ch.items()


if __name__ == '__main__':
    from random import randrange
    WIDTH, HEIGHT = 500, 500
    MAX = 1000
    tree = QTree(AABB(0, 0, WIDTH, HEIGHT))
    for _ in range(MAX):
        tree.insert(Vec(randrange(0, WIDTH/2), randrange(0, HEIGHT/2)))
    print(tree.string_view())

    print(f"q: {list(tree.query(AABB(0, 0, WIDTH/4, HEIGHT/4)))}")

    assert(len(list(tree.items())) == MAX) # We're still not capturing everything?
    # Everything bounded
    print(len(list(tree.query(AABB(-1, -1, WIDTH+1, HEIGHT+1)))))
    # Nothing bounded
    assert(not list(tree.query(AABB(-1, -1, -WIDTH, -HEIGHT))))

    master = tk.Tk()
    master.title("QTree Python")
    paint = tk.Canvas(master, width=WIDTH, height=HEIGHT)
    paint.pack()
    paint.create_rectangle(0, 0, WIDTH, HEIGHT, fill='black')
    tree.draw(paint)

    tk.mainloop()
