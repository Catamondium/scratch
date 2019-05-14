#!/usr/bin/env python3
from contextlib import redirect_stdout
from io import StringIO
import sys


class Indent:
    def __init__(self):
        self.stream = StringIO()

    def __enter__(self):
        sys.stdout = self.stream

    def __exit__(self, a, b, c):
        lines = self.stream.getvalue().split('\n')
        sys.stdout = sys.__stdout__
        for line in lines:
            print(f"\t{line}", end='')
        return False  # rethrow exception


with Indent():
    print("ass")
