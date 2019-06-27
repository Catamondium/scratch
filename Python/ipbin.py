#!/usr/bin/env python3
import sys


def transIP(decIp):
    segs = decIp.split('.')
    bins = [bin(int(x, 10))[2:].zfill(8) for x in segs]
    return ".".join(bins)


if __name__ == "__main__":
    for arg in sys.argv[1:]:
        print(transIP(arg))
