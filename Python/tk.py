#!/usr/bin/env python3
from tkinter import *
from functools import partial


class App(Frame):
    def __init__(self, master=None):
        super().__init__(master)
        self.pack()

        self.top = Frame(master=self.master)
        self.bottom = Frame(master=self.master)
        self.top.pack()
        self.bottom.pack()

        self.num = Label(master=self.top)
        self.num.pack()

        self.var = IntVar()
        self.var.set(0)
        self.num["textvariable"] = self.var

        self.dec = Button(master=self.bottom, text="-", bg="red",
                          command=partial(self.decrement, self))
        self.inc = Button(master=self.bottom, text="+", bg="green",
                          command=partial(self.increment, self))

        self.dec.pack(side="left")
        self.inc.pack(side="left")

    def increment(self, event):
        i = self.var.get()
        i += 1
        self.var.set(i)

    def decrement(self, event):
        i = self.var.get()
        i -= 1
        self.var.set(i)


if __name__ == "__main__":
    app = App()
    app.master.maxsize(100, 400)
    app.mainloop()
