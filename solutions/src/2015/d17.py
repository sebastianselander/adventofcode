import functools

with open("inputs/2015/17.txt") as f:
    s = f.read().strip()

s = list(map(int,s.splitlines()))

s.sort()
s = tuple(s)

@functools.cache
def dp(s, remain,cont):
    if remain == 0:
        return [(cont)]
    if len(s) == 0:
        return []
    if s[-1] > remain:
        return dp(s[:-1], remain,cont)
    cont_ = tuple(list(cont) + [s[-1]])
    return dp(s[:-1], remain, cont) + dp(s[:-1],remain - s[-1], cont_)

res = dp(s,150,())
print(len(res))
xs = [len(tuple(sorted(x))) for x in res]
m = min(xs)
print(len([x for x in xs if x == m]))
