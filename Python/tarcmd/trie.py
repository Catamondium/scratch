#!/usr/bin/env python3
from typing import List, Optional as Maybe
from pathlib import Path
from collections.abc import Mapping
from copy import deepcopy


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

    def __str__(self):
        if self.solved:
            return '/'.join(self.solved)
        else:
            return ''

    def parts(self, pwd) -> Maybe[List[str]]:
        cpwd = deepcopy(pwd)
        if self.solved:
            return self.solved

        if not self._unsolved or self._unsolved == '.':
            return cpwd

        p = Path(self._unsolved)
        parts = [*p.parts]
        resolved = []

        try:
            origin = parts[0]
            if origin in ('~', '/'):
                parts = parts[1:]
            else:
                resolved = cpwd
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


class Trie(Mapping):
    def __init__(self, ch=[]):
        if not ch:
            self.root = True
        else:
            self.root = False
        self.isLeaf = False
        self.ch = ch
        self.children = {}
        self.value = None

    def __repr__(self):
        return f"Trie({', '.join(f'{k}: {v}' for k, v in self.items())}"

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
            yield from ([self.ch] + k for k in chld._genkeys())

    def keys(self):
        """
        yields all keys
        """
        # BUG cheated to avoid rekeying
        yield from (k[1:] for k in self._genkeys())

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
        yield from ((k, self[k]) for k in self.keys())

    def prefixSearch(self, prefix: List[str]):
        """
        yields all keys with prefix
        """
        from copy import deepcopy
        try:

            node = self.getNode(deepcopy(prefix))
            yield from (prefix + k for k in node.keys())
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

    def __eq__(self, other):
        return all(x == y for x, y in zip(self.items(), other.items()))

    def __ne__(self, other):
        return not self == other

    def __getitem__(self, key: List[str]):
        return self.get(key)

    def __setitem__(self, key: List[str], value):
        self.insert(key, value)

    def __delitem__(self, key: List[str]):
        self.remove(key)

    def __len__(self):
        this = 1 if self.isLeaf else 0
        return this + sum(len(chld) for chld in self.children.values())

    def __contains__(self, query: List[str]):
        try:
            node = self.getNode(query)
            return node.isLeaf
        except KeyError:
            return False


if __name__ == "__main__":
    t = Trie()
    assert isinstance(t, Mapping)
    first = Path("A/B/C")
    second = Path("A/B/d")
    paths = {
        first: 1,
        second: '2',
        Path("A/f"): None,
        Path("B/f"): 4,
        Path("C/D/e"): None
    }

    for p, v in paths.items():
        t[list(p.parts)] = v

    print(f"{first.parts} in t => {first.parts in t}")
    assert first.parts in t
    t.print()
    print(len(t))
    print(t)
    del t[first.parts]
    assert first.parts not in t

    print("KEYS")
    for k in t:
        print(k)

    print("PREFIX_SINGLE")
    for k in t.prefixSearch(list(second.parts)):
        print(k)
    print("PREFIX")
    for k in t.prefixSearch(['A']):
        print(k)
