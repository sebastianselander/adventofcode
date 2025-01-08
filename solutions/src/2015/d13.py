from collections import defaultdict
from itertools import permutations


with open("inputs/2015/13.txt") as f:
    s = f.read().strip()

s = s.splitlines()

gr = defaultdict(dict)
for line in s:
    line = line.split()
    fr, ty, c, to = line[0], line[2], line[3], line[-1]
    to = to[:-1]
    if ty == "lose":
        c = -int(c)
    else:
        c = int(c)
    gr[fr][to] = c


def cost(xs):
    xs = list(xs)
    total = 0
    ys = xs[1:]
    ys.append(xs[0])
    for person_left,person_right in list(zip(xs,ys)):
        total += gr[person_left][person_right]
        total += gr[person_right][person_left]
    return total

combs = list(permutations(gr.keys()))
part1 = 0
for x in combs:
    part1 = max(part1, cost(x))

keys = [x for x in gr.keys()]
for k in keys:
    gr[k]["Me"] = 0
    gr["Me"][k] = 0

combs = list(permutations(gr.keys()))
part2 = 0
for x in combs:
    part2 = max(part2,cost(x))

print(part1)
print(part2)
