import functools

with open("../../../inputs/2015/07.txt") as f:
    s = f.read().strip()

wires = {}

for line in s.split("\n"):
    lhs,rhs = line.split(" -> ")
    lhs = lhs.split()
    if "AND" in lhs:
        wires[rhs] = ["AND", lhs[0], lhs[2]]
    elif "OR" in lhs:
        wires[rhs] = ["OR", lhs[0], lhs[2]]
    elif "LSHIFT" in lhs:
        wires[rhs] = ["LSHIFT", lhs[0], lhs[2]]
    elif "RSHIFT" in lhs:
        wires[rhs] = ["RSHIFT", lhs[0], lhs[2]]
    elif "NOT" in lhs:
        wires[rhs] = ["NOT", lhs[1]]
    else:
        wires[rhs] = lhs


@functools.cache
def eval(reg,part2):
    if part2 is not None and reg == "b": return part2 
    if reg.isdigit(): return (int(reg))
    v = wires[reg]
    if v[0] == "AND":
        return eval(v[1],part2) & eval(v[2],part2)
    elif v[0] == "OR":
        return eval(v[1],part2) | eval(v[2],part2)
    elif v[0] == "LSHIFT":
        return eval(v[1],part2) << eval(v[2],part2)
    elif v[0] == "RSHIFT":
        return eval(v[1],part2) >> eval(v[2],part2)
    elif v[0] == "NOT":
        return eval(v[1],part2) ^ 65535
    else:
        return eval(v[0],part2)

res = eval("a",None)
print(res)
print(eval("a",res))
