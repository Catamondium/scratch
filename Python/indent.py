#!/usr/bin/env python3
from contextlib import ContextDecorator
from io import StringIO
import sys


class Indent(ContextDecorator):
    def __init__(self, indent='\t'):
        self.stream = StringIO()
        self.indent = indent

    def __enter__(self):
        self.old = sys.stdout
        sys.stdout = self.stream

    def __exit__(self, a, b, c):
        lines = self.stream.getvalue().split('\n')
        if lines[-1] == '':
            del lines[-1]
        sys.stdout = self.old
        newout = '\n'.join([self.indent + l for l in lines])
        print(newout)
        return False  # rethrow exceptions


@Indent("iFn ")
def indentfunc(string):
    print(string)


if __name__ == "__main__":
    with Indent(' '*4):
        print("ass")
        with Indent(' '*4):
            print("bass")
    indentfunc("ABCDEF")
