from utils import *
with open("inputs/2015/18.txt") as f:
    s = f.read().strip()

gr,N,M = grid(s)

def update(g, p2=False):
    g_ = [[x for x in y] for y in g]
    for cr in range(N):
        for cc in range(M):
            if p2 and (cr,cc) in [(0,0), (N-1,M-1), (N-1,0), (0,M-1)]:
                continue
            n = 0
            for nr,nc in neighbors(cr,cc,N,M):
                if g[nr][nc] == '#': 
                    n += 1
            c = '.'
            if g[cr][cc] == '#' : 
                if n in [2,3]:
                    c = '#'
            if g[cr][cc] == '.':
                if n == 3:
                    c = '#'
            g_[cr][cc] = c
    return g_

gr1 = grid(s)[0]
gr2 = grid(s)[0]
for _ in range(100):
    gr1 = update(gr1)
    gr2 = update(gr2, p2=True)

ans1 = 0
ans2 = 0
for x in range(N):
    for y in range(M):
        if gr1[x][y] == '#':
            ans1 += 1
        if gr2[x][y] == '#':
            ans2 += 1

print(ans1)
print(ans2)
