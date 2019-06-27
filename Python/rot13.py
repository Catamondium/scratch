#!/usr/bin/env python3
from string import ascii_uppercase, ascii_lowercase
from collections import deque
trueAscii = ascii_lowercase + ascii_uppercase


def rot13(source):
    out = ""
    for ch in source:
        newch = ch
        if ch in trueAscii:
            newch = trueAscii.index(ch) + 13
            newch %= len(trueAscii)
            newch = trueAscii[newch]
        out += newch
    assert(len(out) == len(source))
    return out


if __name__ == "__main__":
    source = "Lbh penpxrq gur pbqr!"
    print(rot13(source))
    print(f'\n{trueAscii}\n{rot13(trueAscii)}')
