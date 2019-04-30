from multiprocessing import Pool


def fun(arg):
    return arg * 2


if __name__ == "__main__":
    with Pool(processes=20) as p:
        data = p.map(fun, set([2, 4, 6, 8]))
    print(data)
