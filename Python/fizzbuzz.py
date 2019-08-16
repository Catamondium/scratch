#!/usr/bin/env python3
import sys

if (len(sys.argv) != 1
        and len(sys.argv) % 2 == 1):  # Parse to dict

    dict = {}
    for i in range(1, len(sys.argv), 2):
        dict.update({int(sys.argv[i]): sys.argv[i+1]})
else:  # Default example
    dict = {
        3: 'Fizz',
        5: 'Buzz',
        7: 'Baz'
    }

# Main
for i in range(1, 101):
    hits = ""
    for k, v in dict.items():
        if (i % k == 0):
            hits += v
    print(hits or i)
