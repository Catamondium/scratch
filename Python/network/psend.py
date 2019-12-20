#!/usr/bin/env python3

from os import fork
from socket import socketpair
from ctypes import c_ssize_t, sizeof
from collections import namedtuple
from pickle import loads, dumps

rx, tx = socketpair()

"""
Message format:
<SIZE/TERM>[BODY]
SIZE == ssize_t
where -1 := end of segment

BODY == pickled data, arbitrary size
"""

A = namedtuple("A", ["x", "y"])

def f(x):
    return x * 2

pid = fork()
if pid == 0:
    del tx # child
    while True:
        buf = rx.recv(sizeof(c_ssize_t))
        expected = c_ssize_t.from_buffer_copy(buf)
        if expected.value == -1:
            print("child exiting")
            break

        jar = rx.recv(expected.value)
        print(f"<- {loads(jar)}[{expected.value}]")

else:
    del rx # parent
    # Can't send lambdas, must presumably share Decls with reciever
    data = [{"abc": 1, "def": 2}, A(255, 300), f]
    for datum in data:
        jar = dumps(datum)
        sent = c_ssize_t(len(jar))
        tx.send(sent)
        tx.send(jar)
        print(f"-> {datum}[{sent.value}]")

    tx.send(c_ssize_t(-1))
    print("-> [-1]")
    print("Terminated comms")
