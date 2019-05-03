#!/usr/bin/env python3
import tkinter as tk
from functools import partial


class App(tk.Frame):
    def __init__(self, master=None):
        super().__init__(master)
        self.pack()

        self.top = tk.Frame(master=self.master)
        self.bottom = tk.Frame(master=self.master)
        self.top.pack()
        self.bottom.pack()

        self.num = tk.Label(master=self.top)
        self.num.pack()

        self.var = tk.IntVar()
        self.var.set(0)
        self.num["textvariable"] = self.var

        self.dec = tk.Button(master=self.bottom, text="-", bg="red",
                             command=partial(self.decrement, self))
        self.inc = tk.Button(master=self.bottom, text="+", bg="green",
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
    app.master.title("Simple")
    app.mainloop()
