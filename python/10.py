inp = open("../inputs/2024/09.txt").read()
inp = "".join(inp.splitlines())
inp = "2333133121414131402"

expanded = []
small = []
index = 0
for i in range(0, len(inp), 2):
    file = inp[i]
    file = int(file)
    for j in range(file): 
        expanded.append(index)

    small.append((file, index))
    if i+1 < len(inp):
        free = inp[i+1]
        free = int(free)
        for j in range(free): 
            expanded.append('.')
        if free != 0: small.append((free, '.'))
    index += 1

i, j = 0, len(expanded) - 1
while(i <= j):
    while(expanded[i] != '.'):
        i += 1
    while(expanded[j] == '.'):
        j -= 1
    if i <= j: 
        expanded[i], expanded[j] = expanded[j], expanded[i]


i, j = 0, len(small)
while(i <= j):
    j -= 1
    while i <= j:
        if small[i][1] == '.' and small[i][0] >= small[j][0]: 
            break
        i += 1

    if i <= j and small[j][1] != '.': 
        diff = small[i][0] - small[j][0]
        if diff == 0:
            small[i], small[j] = small[j], small[i]
        elif diff > 0: 
            small[i], small[j] = small[j], (small[j][0], small[i][1])
            small.insert(i+1,(diff,'.'))
    i = 0 

p1 = 0
for i in range(len(expanded)):
    curr = expanded[i] 
    if curr != '.':
        p1 += curr * i

print(p1)
print(small)
p2 = 0
k = 1
for i in range(len(small)):
    curr, item = small[i]
    print((curr,item),k)
    if item != '.':
        for _ in range(curr): 
            k += 1
            p2 += k * item
    else:
        k += curr

print(p2)
