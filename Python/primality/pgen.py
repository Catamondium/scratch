#!/usr/bin/python3
primes = [2] # 2 seeds
def isprime(i):
    truth = True # assumption
    for prime in primes:
        if x % prime == 0:
            truth = False
    return truth


n = 10 # get first 10 primes

i = 1
x = 2
while(i < n):
    if isprime(x):
        primes.append(x)
        i += 1
    x += 1

print("%r" % primes)
