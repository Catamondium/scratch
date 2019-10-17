#!/usr/bin/env python3
from typing import List

class Trie:
    def __init__(self, ch=[]):
        if not ch:
            self.root = True
        else:
            self.root = False
        self.isLeaf = False
        self.ch = ch
        self.children = {}
        self.value = None

    def insert(self, strings: List[str], value=None):
        if not strings:
            self.isLeaf = True
            self.value = value
            return
        try:
            self.children[strings[0]].insert(strings[1:], value)
        except KeyError:
            chld = Trie(strings[0])
            chld.insert(strings[1:], value)
            self.children[strings[0]] = chld

    def get(self, strings: List[str]):
        if not strings:
            if self.isLeaf:
                return self.value
            raise KeyError
        
        return self.children[strings[0]].get(strings[1:])

    def remove(self, strings: List[str]):
        if not strings:
            if self.isLeaf:
                self.isLeaf = False
                self.value = None
                return
            raise KeyError
        
        self.children[strings[0]].get(strings[1:])

    def keys(self, _prec=[]):
        """
        yields all keys.\n
        _prec is for internal use
        """
        if self.isLeaf:
            yield [*_prec[1:], self.ch]

        for chld in self.children.values():
            yield from chld.keys([*_prec, self.ch])

    def values(self, _prec=[]):
        """
        yields all values.\n
        _prec is for internal use
        """
        if self.isLeaf:
            yield self.value

        for chld in self.children.values():
            yield from chld.values([*_prec, self.ch])

    def items(self, _prec=[]):
        """
        yields all key,value pairs.\n
        _prec is for internal use
        """
        if self.isLeaf:
            yield ([*_prec[1:], self.ch], self.value)

        for chld in self.children.values():
            yield from chld.items([*_prec, self.ch])

    def prefixSearch(self, prefix: List[str], _prec=[]):
        """
        yields all keys with prefix.\n
        _prec is for internal use
        """
        if prefix == []:
            # prefix exhasuted, match all
            yield from self.keys(_prec)
        else:
            try:
                # prefix not exhausted, traverse further
                chld = self.children[prefix[0]]
                yield from chld.prefixSearch(prefix[1:], [*_prec, self.ch])
            except IndexError:
                yield None
            except KeyError:
                yield None

    def print(self, indent=""):
        if self.root:
            print("R")
        else:
            if self.value:
                leaf = f"{{{self.ch} : {repr(self.value)}}}"
            else:
                leaf = self.ch
            print(f"{indent}-{leaf}")
        if len(self.children) != 0:
            print(f"{indent}\\")
        indent += " |"
        for chld in self.children.values():
            chld.print(indent)

    def __iter__(self):
        return self.keys()

    def __setitem__(self, key: List[str], value):
        self.insert(key, value)

    def __getitem__(self, key: List[str]):
        return self.get(key)

    def __contains__(self, query: List[str]):
        if not query:
            if self.isLeaf:
                return True
            else:
                return False

        print(query)
        try:
            return query[1:] in self.children[query[0]]
        except KeyError:
            return False
        return False


if __name__ == "__main__":
    t = Trie()
    from pathlib import Path
    first = Path("A/B/C")
    paths = {
        first: 1,
        Path("A/B/d"): '2',
        Path("A/f"): None,
        Path("B/f"): 4
    }

    for p,v in paths.items():
        t[list(p.parts)]= v

    print(f"{first.parts} in t => {first.parts in t}")
    t.print()
