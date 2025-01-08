with open("inputs/2015/14.txt") as f:
    s = f.read().strip()

d = {}
p = {}
for line in s.splitlines():
    line = line.split()
    name, speed, time, rest = line[0], line[3], line[6], line[13]
    d[name] = [int(speed),int(time),int(rest), int(time), int(rest)]
    p[name] = 0

def move(name):
    times=[]
    i=0
    ix=3
    while i<2503:
        speed, time, rest, _, _ = d[name]
        if d[name][ix] == 0:
            if ix==3:
                d[name][ix] = time
                ix = 4
            elif ix==4:
                d[name][ix] = rest
                ix = 3
            else:
                assert False
        prev = 0 if i == 0 else times[-1]
        if ix==3:
            times.append(prev + speed)
        elif ix==4:
            times.append(prev)
        d[name][ix] -= 1
        i += 1
    return times


xs = 0
m = {}
for x in d.keys(): 
    m[x] = move(x)
    xs = max(xs,m[x][-1])
print(xs)

ys = [0] * 9
for x in zip(*m.values()):
    m_v = max(x)
    m_i = [i for i,x in enumerate(x) if x == m_v]
    for i in m_i:
        ys[i] += 1
print(max(ys))


