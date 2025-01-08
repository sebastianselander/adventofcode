with open("day3.txt") as f:
    file = f.read()

x1 = 0
y1 = 0

x2 = 0
y2 = 0

b = True

visited = set({(0,0)})

for c in file:

    if c == '<': 
        x1 -= 1
    if c == '>': 
        x1 += 1
    if c == 'v': 
        y1 += 1
    if c == '^': 
        y1 -= 1
    visited.add((x1,y1))
print(len(visited))

visited = set({0,0})
x1 = 0
y1 = 0

for c in file:
    if c == '<':
        if b:
            x1 -= 1
        else:
            x2 -= 1
    if c == '>':
        if b:
            x1 += 1
        else:
            x2 += 1
    if c == 'v':
        if b:
            y1 += 1
        else:
            y2 += 1
    if c == '^':
        if b:
            y1 -= 1
        else:
            y2 -= 1
    if b:
        x = x1
        y = y1
    else:
        x = x2
        y = y2
    visited.add((x,y))
    b = not b

print(len(visited))
