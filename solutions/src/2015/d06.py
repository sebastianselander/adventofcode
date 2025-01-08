import re
from utils import *

with open("../../../inputs/2015/06.txt") as f:
    s = f.read().strip()

s = s.split("\n")
g1 = [[0 for _ in range(1000)] for _ in range(1000)]
g2 = [[0 for _ in range(1000)] for _ in range(1000)]

for line in s:
    a, b, c, d = nums(line)
    if line.startswith("turn on"):
        for i in range(a, c+1):
            for j in range(b, d+1):
                g1[i][j] = 1
                g2[i][j] += 1
    if line.startswith("turn off"):
        for i in range(a, c+1):
            for j in range(b, d+1):
                g1[i][j] = 0
                g2[i][j] = max(g2[i][j]-1, 0)
    if line.startswith("toggle"):
        for i in range(a, c+1):
            for j in range(b, d+1):
                g1[i][j] = int(not g1[i][j])
                g2[i][j] += 2

part1 = 0
part2 = 0
for i in g1:
    for j in i:
        part1 += j
for i in g2:
    for j in i:
        part2 += j

print(part1)
print(part2)
