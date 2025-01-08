import re

def readfile(year,day):
    year = str(year)
    day = "0" if day < 10 else "" + str(day)
    with open("inputs/" + year +"/" + day + ".txt") as f:
        return f.read().strip()

def nums(x):
    return list(map(int, re.findall(r"-?\d+", x)))

def neighbors(x, y, N=None, M=None):
    for dx, dy in [ (-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1), ]:
        nx = x + dx
        ny = y + dy
        if N is not None and M is not None:
            if 0 <= nx < N and 0 <= ny < M:
                yield nx,ny
        else:
            yield nx,ny


def cardinal(x, y):
    for dx, dy in [(-1, 0), (0, -1), (0, 1), (1, 0)]:
        yield x + dx, y + dy

def grid(s):
    gr = [[x for x in y] for y in s.splitlines()]
    return gr, len(gr), len(gr[0])
