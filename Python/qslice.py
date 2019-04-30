import math
import statistics as stat
import sys


def Qn(n, q):
    if(q == 0):
        print("MathError: divide by 0")
        sys.exit(1)
    return len(n) / q


def Qslice(n, q, x):
    half = x/2
    return n[math.floor(Qn(n, q) - half):math.floor(Qn(n, q) + half)]


# main
if(len(sys.argv) < 5):
    print("Error: #lower #upper #quartile #range expected")
    sys.exit(1)

in_ = sys.argv[1::]
in_ = [int(x) for x in in_]

seq = [n for n in range(in_[0], in_[1] + 1)]
print(
    f"Within {in_[3] / 2:0g} of Q{in_[2]:01d}:\t{Qslice(seq, in_[2], in_[3])}")
sys.exit(0)
