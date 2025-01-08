from utils import *

s = readfile(2015, 20)
s = int(s)


def factors(n, d):
    i = 1
    yield n * d
    while i <= int(n // 2 + 1):
        if n % i == 0:
            yield i * d
        i += 1


def presents(x, d, p2=False):
    ans = 0
    for y in factors(x, d):
        if p2: 
            if x in [x * y//d for x in range(1, 51)]:
                ans += y
        else:
            ans += y

    return ans


i = 600_000
ans1 = 0
ans2 = 0
p1 = False
p2 = False
while True:
    if p1 and p2:
        break
    ans1 = ans1 if p1 else max(ans1, presents(i,10))
    ans2 = ans2 if p2 else max(ans2, presents(i,11,p2=True))
    if ans1 >= s:
        p1 = True
        ans1 = i
    if ans2 >= s:
        p2 = True
        ans2 = i
    i += 10

print(ans1)
print(ans2)
