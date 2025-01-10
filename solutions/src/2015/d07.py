from functools import cache

with open("../../../inputs/2015/07.txt") as f:
    s = f.read().strip()

wires = {}

for line in s.split("\n"):
    lhs, rhs = line.split(" -> ")
    lhs = lhs.split()
    match lhs:
        case [l, "AND", r]:
            wires[rhs] = ["AND", l, r]
        case [l, "OR", r]:
            wires[rhs] = ["OR", l, r]
        case [l, "LSHIFT", r]:
            wires[rhs] = ["LSHIFT", l, r]
        case [l, "RSHIFT", r]:
            wires[rhs] = ["RSHIFT", l, r]
        case ["NOT", v]:
            wires[rhs] = ["NOT", v]
        case _:
            wires[rhs] = lhs

@cache
def eval(reg, part2):
    if part2 is not None and reg == "b":
        return part2
    if reg.isdigit():
        return int(reg)
    v = wires[reg]
    match v[0]:
        case "AND":
            return eval(v[1], part2) & eval(v[2], part2)
        case "OR":
            return eval(v[1], part2) | eval(v[2], part2)
        case "LSHIFT":
            return eval(v[1], part2) << eval(v[2], part2)
        case "RSHIFT":
            return eval(v[1], part2) >> eval(v[2], part2)
        case "NOT":
            return eval(v[1], part2) ^ 65535
        case _:
            return eval(v[0], part2)


res = eval("a", None)
print(res)
print(eval("a", res))
