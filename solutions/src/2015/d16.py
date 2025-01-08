with open("inputs/2015/16.txt") as f:
    s = f.read().strip()

def solve(part2):
    xs = [0] * 501
    for l in s.splitlines():
        t = 0
        l = list(map(lambda x: x.strip(","), l.split()))
        n = int(l[1].strip(":"))
        def index(x,t,p2=None):
            x += ":"
            if x not in l:
                return 0
            else:
                have = int(l[1 + l.index(x)]) 
                if p2 == 1:
                    return 1 if t < have else 0
                elif p2 == 0:
                    return 1 if t > have else 0
                else:
                    return 1 if have == t else 0
        if part2:
            t += index("cats",7,p2=1)
            t += index("trees",3,p2=1)
            t += index("pomeranians",3,p2=0)
            t += index("goldfish",5,p2=0)
        else:
            t += index("cats",7)
            t += index("trees",3)
            t += index("pomeranians",3)
            t += index("goldfish",5)

        t += index("children",3)
        t += index("samoyeds",2)
        t += index("akitas",0)
        t += index("vizslas",0)
        t += index("cars",2)
        t += index("perfumes",1)

        xs[n] = t

    y = max(xs)

    return list(k for k,x in enumerate(xs) if x == y)[0]

print(solve(False))
print(solve(True))
