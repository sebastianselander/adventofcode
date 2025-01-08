import functools
from utils import *
import sys

sys.setrecursionlimit(10**9)
from collections import defaultdict
s = readfile(2015,19) 

xs, ys = s.split("\n\n")
g = defaultdict(list)

for l in xs.splitlines():
    k,v = l.split(" => ")
    g[k].append(v)

d = set()

for i in range(len(ys)):
    if i < len(ys):
        if ys[i:i+2] in g:
            new = g[ys[i:i+2]]
            for n in new:
                d.add(ys[:i] + n + ys[i+2:])
        if ys[i] in g:
            new = g[ys[i]]
            for n in new:
                d.add(ys[:i] + n + ys[i+1:])
print(len(d))

g_ = defaultdict(list)
for k,l in g.items():
    for v in l:
        g_[v].append(k)

seen = set()
ans = float("inf")
@functools.cache
def construct(e,depth=0):
    global ans
    if ans < float("inf"):
        return ans
    if e in seen: 
        return float("inf")
    seen.add(e)
    if e == "e":
        return depth
    visit = set()
    for i in range(len(e)):
        for k,l in g_.items():
            print(k,l)
            for v in l:
                size = len(k)
                if e[i:size+i] == k:
                    visit.add(e[:i] + v + e[i+size:])
    for x in visit:
        ans = min(ans,construct(x,depth+1))
    return ans

print(construct(ys))
