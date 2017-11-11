from functools import reduce
import time

with open('data.txt', 'r') as f:
    data = f.read()

a = list(map(int, data.split()))
b = list(range(0, 2000000))

def f(a, b):
    return a + b

def example0():
    n = all(it > 4 for it in
            map(lambda it: it * 2,
                filter(lambda it: it % 4 > 1,
                    map(lambda it: it[0] + it[1],
                        zip(a, b)))))

def example1():
    o = reduce(lambda it, memo: it + memo,
            map(lambda it: it - 7,
                map(lambda it: f(it, it),
                    a)))

if __name__ == '__main__':
    import timeit
    # start = time.clock()
    # example0()
    # finish = time.clock()
    # print(finish - start)
    # start = time.clock()
    # example1()
    # finish = time.clock()
    # print(finish - start)
    print(timeit.timeit("example0()", setup="from __main__ import example0", number=3) / 3.0)
    print(timeit.timeit("example1()", setup="from __main__ import example1", number=3) / 3.0)
