from collections import defaultdict
from copy import copy
from itertools import groupby


with open("../../../inputs/2015/10.txt") as f:
    s = f.read().strip()

s = [x for x in s]

def group(xs):
    return [list(v) for _, v in groupby(xs)]


def run(xs):
    ys = group(xs)
    zs = []
    for v in ys:
        zs.append(str(len(v)))
        zs.append(v[0])
    return zs

ans1 = 0
for x in range(50):
    if x == 40:
        ans1 = len(s)
    s = run(s)
ans2 = len(s)
print(ans1)
print(ans2)
