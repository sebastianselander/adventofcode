with open("../../../inputs/2015/08.txt") as f:
    s = f.read().strip()

s = s.splitlines()

res = 0
for line in s:
    line = line[1:len(line)-1]
    x = 0
    j = 0
    while j < len(line):
        if line[j:j+2] == "\\x":
            j += 4
        elif line[j] == "\\":
            j+=2
        else: 
            j += 1
        x += 1
    res += 2 + len(line) - x
print(res)


res = 0
for line in s:
    c = 0
    for l in line:  
        if l == "\"":
            c += 2
        elif l == "\\":
            c += 2
        else:
            c += 1
    c += 2
    res += c-(len(line))

print(res)
