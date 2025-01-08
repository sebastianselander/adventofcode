with open("../../../inputs/2015/11.txt") as f:
    s = f.read().strip()


def is_valid(s):
    good1 = True
    good2 = False
    good3 = set()
    for i, v in enumerate(s):
        if v in "iol":
            good1 = False
        if i < len(s) - 2:
            x, y, z = list(map(ord, s[i : i + 3]))
            if x + 2 == y + 1 == z:
                good2 = True
        if i < len(s) - 1:
            x, y = s[i : i + 2]
            if x == y:
                good3.add(i)
                good3.add(i + 1)
    return good1 and good2 and len(good3) >= 4


def increment_str(s):
    def increment_char(c):
        if c == "z":
            return True, "a"
        else:
            return False, chr(1 + ord(c))

    xs = [x for x in s]
    s = ""
    for i in range(len(xs) - 1, -1, -1):
        carry, n = increment_char(xs[i])
        xs[i] = n
        if not carry:
            break
    for x in xs:
        s += x
    return s


while not is_valid(s):
    s = increment_str(s)

print(s)

s = increment_str(s)
while not is_valid(s):
    s = increment_str(s)

print(s)
