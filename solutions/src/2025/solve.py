from collections import defaultdict
from pulp import *

content = [eval(x) for x in open("./solutions/src/2025/python.txt").read().splitlines()]
def solve(c):
    sum = 0
    model = LpProblem("Problem", LpMinimize)
    presses = c[:-1]
    joltage = c[-1]
    variables = [LpVariable("x" + str(x+1), cat="Integer", lowBound=-0.5) for x in range(0,len(presses))]
    model += lpSum(variables)

    d = defaultdict(list)
    for i in range(0, len(joltage)):
        for ii, press in enumerate(presses):
            if i in press:
                d[i].append(ii)
    for index, vars in d.items():
        result = joltage[index]
        model += lpSum(variables[x] for x in vars) == result

    model.solve(PULP_CBC_CMD(msg=0))
    for v in variables:
        sum += value(v)
    return sum

res = 0
for c in content:
    res += solve(c)
print(res)
