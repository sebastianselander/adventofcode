from collections import defaultdict
from copy import copy

with open("../../../inputs/2015/09.txt") as f:
    s = f.read().strip()

s = s.splitlines()

g = defaultdict(set)
for line in s:
    fr, r = line.split(" to ")
    to, c = r.split(" = ")
    c = int(c)
    g[fr].add((to, c))
    g[to].add((fr, c))


def path(start,vis,f,d):
    ans = d
    for n,c in g[start]:
        if n not in vis:
            vis_ = copy(vis)
            vis_.add(n)
            ans = f(ans, c + path(n,vis_,f,d))
    return ans if ans != inf else 0
            
inf = float("inf")
ans1 = inf
ans2 = 0
for e in g.keys():
    ans1 = min(ans1,path(e,set([e]),min,inf))
    ans2 = max(ans2, path(e,set([e]),max,0))

print(ans1)
print(ans2)
