import asyncio
from time import time
from functools import wraps

DELAY = 0.01
MAX = 200


def async_time(coroutine):
    @wraps(coroutine)
    async def deco(*args, **kwargs):
        t = Timer()
        with t:
            ret = await coroutine(*args, **kwargs)
        return t.elapse, ret
    return deco


class Timer:
    def __init__(self):
        self._start = None
        self.elapse = None

    def begin(self):
        self._start = time()

    def term(self):
        end = time()
        self.elapse = end - self._start
        return self.elapse

    def __enter__(self):
        self.begin()

    def __exit__(self, exc_type, exc_val, exc_tb):
        self.term()


async def multiple(x, y):
    await asyncio.sleep(DELAY)
    return x % y == 0


@async_time
async def divisors(x):
    ret = [1]
    for b in range(2, x):
        if await multiple(x, b):
            ret.append(b)
    return ret


async def main():
    tests = []
    for i in range(1, MAX):
        tests.append(divisors(i))
    return await asyncio.gather(*tests)


if __name__ == '__main__':
    from pprint import pprint
    meta = Timer()
    with meta:
        resp = asyncio.run(main())
    pprint(list(map(lambda x: (round(x[0], 2), x[1]), resp)))
    seq = sum(map(lambda x: x[0], resp))
    print(f"Total run: async {meta.elapse:.2}s, sequential {round(seq, 2)}s")
