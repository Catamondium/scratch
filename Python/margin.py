#!/usr/bin/env python3


def _line_stripMargin(line):
    loc = line.find('|')
    if loc == -1:
        return line
    else:
        return line[loc+1:]


def stripMargin(thing):
    striplines = [_line_stripMargin(x) for x in thing.splitlines()]
    i = 1 if striplines[0] == "" else 0
    e = -2 if striplines[-1] == "" else - 1
    return '\n'.join(striplines[i:e])


print(stripMargin("""
    |
    |Remove margin
    | And the next
    |  and the next * 2
    |   embedded |
    |
    """))
