#!/usr/bin/env python3
# Generator testing
sample = 1000000
Result = []


def firstn_A(n):
    """List generation method"""
    num, nums = 0, []
    while num < n:
        nums.append(num)
        num += 1
    return nums


Result.append(sum(firstn_A(sample)))


def firstn_G(n):
    """Generator function proper"""
    num = 0
    while num < n:
        yield num   # Return
        num += 1    # Shift


Result.append(sum(firstn_G(sample)))

print(Result)

# List comprehension methods

doubles_L = [2 * n for n in range(50)]  # As List
doubles_G = (2 * n for n in range(50))  # As generator
print(doubles_L)
print(doubles_G)
