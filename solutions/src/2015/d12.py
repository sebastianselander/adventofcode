import re
import json

with open("../../../inputs/2015/12.txt") as f:
    s = f.read().strip()

print(sum(map(int,(re.findall(r"-?\d+",s)))))

def numbers(x):
    s = 0
    if type(x) == int:
        s += int(x)
    elif type(x) == list:
        for v in x:
            s += numbers(v)
    elif type(x) == dict:
        if "red" in x.values():
            return 0
        for v in x.values():
            s += numbers(v)
    return s 

print(numbers(json.loads(s)))
