import re
import itertools

with open("inputs/2015/15.txt") as f:
    s = f.read().strip()


s = s.splitlines()

ing = []
for l in s:
    ing.append(list(map(int, re.findall(r"-?\d+", l))))

def total(xs, p2=False):
    ans = 1
    ys = []
    p1 = ing
    for k, v in list(zip(xs, p1)):
        ys.append((list((map(lambda x: x * k, v)))))
    ys = list(zip(*ys))
    if p2 and sum(ys[-1]) != 500:
        return 0
    for l, r, x, y in ys[:-1]:
        ans *= l + r + x + y if l + r + x + y >= 0 else 0
    return ans



ans1 = 0
ans2 = 0
for x in (x for x in itertools.permutations(range(0,101), 4) if sum(x) == 100):
    ans1 = max(ans1, total(x))
    ans2 = max(ans2, total(x,p2=True))
import
print(ans2)
