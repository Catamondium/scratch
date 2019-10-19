#!/usr/bin/env python3
from typing import List, Optional as Maybe
from pathlib import Path


class TPath:
    """Trie path, submit to Trie w/ #parts"""

    def __init__(self, string: str):
        self._unsolved = string
        self.solved = None
        self._pending = []

    def __div__(self, npart):
        if self.solved:
            self.solved.append(npart)
        else:
            self._pending.append(npart)

    def __repr__(self):
        if self.solved:
            return f"TPath({self.solved})"
        else:
            return f"TPath(\'{self._unsolved}\')"

    def parts(self, pwd) -> Maybe[List[str]]:
        if self.solved:
            return self.solved

        if not self._unsolved or self._unsolved == '.':
            return pwd

        p = Path(self._unsolved)
        parts = [*p.parts]
        resolved = []

        try:
            origin = parts[0]
            if origin in ('~', '/'):
                parts = parts[1:]
            else:
                resolved = pwd
        except IndexError:
            pass

        for part in parts:
            if part in ('.', '~'):
                # Ignore null move
                continue
            elif part == '..' and resolved:
                # Remove previous, obeying root
                resolved.pop()
            else:
                resolved.append(part)
        if self._pending:
            resolved += self._pending
            self._pending = None
        self.solved = resolved
        return self.solved


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
        """
        Inserts value into trie at strings
        equivalent to Trie()[strings] = value
        """
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
        """
        Fetch value at strings
        equivalent to Trie()[strings]
        """
        node = self.getNode(strings)
        if node.isLeaf:
            return node.value
        raise KeyError

    def getNode(self, strings: List[str]):
        """
        Get a subtree reference at strings
        """
        if not strings:
            return self
        else:
            return self.children[strings[0]].getNode(strings[1:])

    def remove(self, strings: List[str]):
        """
        Remove a value at strings
        equiv del obj[strings]
        """
        node = self.getNode(strings)
        if node.isLeaf:
            node.isLeaf = False
            self.value = None
        else:
            raise KeyError

    def _genkeys(self):
        # FIXME I shouldn't be needed
        if self.isLeaf:
            yield [self.ch]

        for chld in self.children.values():
            yield from map(lambda x: [self.ch] + x, chld._genkeys())

    def keys(self):
        """
        yields all keys
        """
        # BUG cheated to avoid rekeying
        yield from map(lambda x: x[1:], self._genkeys())

    def values(self):
        """
        yields all values
        """
        if self.isLeaf:
            yield self.value

        for chld in self.children.values():
            yield from chld.values()

    def items(self):
        """
        yields all key,value pairs
        """
        yield from map(lambda k: (k, self[k]), self.keys())

    # TODO getNode refactor & purify keys/values/items
    def prefixSearch(self, prefix: List[str]):
        """
        yields all keys with prefix
        """
        try:
            node = self.getNode(prefix)
            yield from map(lambda x: prefix + x, node.keys())
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

    def __bool__(self):
        if self.isLeaf:
            return True
        else:
            return any(map(bool, self.children))

    def __getitem__(self, key: List[str]):
        return self.get(key)

    def __setitem__(self, key: List[str], value):
        self.insert(key, value)

    def __delitem__(self, key: List[str]):
        self.remove(key)

    def __contains__(self, query: List[str]):
        try:
            node = self.getNode(query)
            return node.isLeaf
        except KeyError:
            return False


if __name__ == "__main__":
    t = Trie()
    first = Path("A/B/C")
    paths = {
        first: 1,
        Path("A/B/d"): '2',
        Path("A/f"): None,
        Path("B/f"): 4,
        Path("C/D/e"): None
    }

    for p, v in paths.items():
        t[list(p.parts)] = v

    print(f"{first.parts} in t => {first.parts in t}")
    assert first.parts in t
    t.print()
    del t[first.parts]
    assert first.parts not in t

    print("KEYS")
    for k in t:
        print(k)

    print("PREFIX")
    for k in t.prefixSearch(['A']):
        print(k)
