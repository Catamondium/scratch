#!/usr/bin/env python3
class Trie:
    def __init__(self, ch=""):
        if ch is None:
            self.root = True
        else:
            self.root = False
        self.isLeaf = False
        self.ch = ch
        self.children = {}
        self.value = None

    def insert(self, string: str, value=None):
        if string == "":
            self.isLeaf = True
            self.value = value
            return
        try:
            self.children[string[0]].insert(string[1:])
        except KeyError:
            chld = Trie(string[0])
            chld.insert(string[1:], value)
            self.children[string[0]] = chld

    def __contains__(self, string: str):
        if string == "" and self.isLeaf:
            return True

        try:
            return string[1:] in self.children[string[0]]
        except KeyError:
            return False
        except IndexError:
            return False
        return False

    def keys(self, prec=""):
        if self.isLeaf:
            yield prec + self.ch

        for chld in self.children.values():
            yield from chld.keys(prec + self.ch)

    def __iter__(self):
        return self.keys()

    def values(self, prec=""):
        if self.isLeaf:
            yield self.value

        for chld in self.children.values():
            yield from chld.values(prec + self.ch)

    def items(self, prec=""):
        if self.isLeaf:
            yield (prec + self.ch, self.value)

        for chld in self.children.values():
            yield from chld.items(prec + self.ch)

    def print(self, indent=""):
        if self.root:
            print("R")
        else:
            if self.value:
                leaf = f"{{{self.ch} : {self.value}}}"
            else:
                leaf = self.ch
            print(f"{indent}-{leaf}")
        if len(self.children) != 0:
            print(f"{indent}\\")
        indent += " |"
        for chld in self.children.values():
            chld.print(indent)


if __name__ == "__main__":
    t = Trie()
    t.insert("ass", 25)
    t.insert("abb")
    t.insert("bbb")
    t.print()
    print("ass" in t)
    print("cass" in t)
    print("as" in t)
    print(list(t.keys()))
    print(list(t.values()))
    print(list(t.items()))
