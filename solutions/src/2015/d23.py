from collections import defaultdict
from utils import *

s = readfile(2015, 23)
s = s.splitlines()

def solve(n):
    i = 0
    reg = defaultdict(int)
    reg["a"] = n
    while i < len(s):
        line = s[i].replace(",","").split()
        match line[0]:
            case "hlf": reg[line[1]] //= 2
            case "tpl": reg[line[1]] *= 3
            case "inc": reg[line[1]] += 1
            case "jmp": i += int(line[1]) - 1
            case "jie": 
                if reg[line[1]] % 2 == 0: i += int(line[2]) - 1
            case "jio":
                if reg[line[1]] == 1: i += int(line[2]) - 1
        i += 1
    return reg["b"]

print(solve(0))
print(solve(1))
