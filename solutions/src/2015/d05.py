with open("../../../inputs/2015/05.txt") as f:
    file = f.read().strip().split()

vowels = "aeiou"

total = 0

for word in file:
    good = False
    vow = 0
    prev = ""
    for c in word:
        if c in vowels:
            vow += 1
        if c == prev:
            good = True
        prev = c
    for e in ["ab", "cd", "pq", "xy"]:
        if e in word:
            good = False
    if good and vow >= 3:
        total += 1

print(total)
res = 0
for word in file:
    good1 = False
    good2 = False
    for i,(x,y) in enumerate(zip(word, word[1:])):
        if x+y in word[i+2:]:
            good1 = True
    for x,y in zip(word, word[2:]):
        if x == y:
            good2 = True
    if good1 and good2:
        res += 1
print(res)
