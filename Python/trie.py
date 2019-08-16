#!/usr/bin/env python3
class Trie:
    def __init__(self, ch=None):
        if ch is None:
            self.root = True
        else:
            self.root = False
        self.ch = ch
        self.children = {}

    def insert(self, string: str):
        if string == "":
            return
        try:
            self.children[string[0]].insert(string[1:])
        except KeyError:
            chld = Trie(string[0])
            chld.insert(string[1:])
            self.children[string[0]] = chld

    def find(self, string: str):
        if string == "":
            return True

        # if self.root:
        try:
            return self.children[string[0]].find(string[1:])
        except KeyError:
            return False
        # else:
        #  return self.children[string[0]].find(string[1:])
        pass

    def print(self, indent=""):
        if self.root:
            print("R")
        else:
            print(f"{indent}-{self.ch}")
        if len(self.children) != 0:
            print(f"{indent}\\")
        indent += " |"
        for chld in self.children.values():
            chld.print(indent)


if __name__ == "__main__":
    t = Trie()
    t.insert("ass")
    t.insert("abb")
    t.insert("bbb")
    t.print()
    print(t.find("ass"))
    print(t.find("cass"))
