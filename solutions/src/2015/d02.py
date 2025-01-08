with open("day2.txt") as f:
    presents = list(map(lambda x: x.split("x"),f.read().split()))

area = 0
total2 = 0

for [l,w,h] in presents:
    l = int(l)
    w = int(w)
    h = int(h)
    area += (2 * l * w + 2 * w * h + 2 * h * l) + min(l * w, w * h, h * l)
    (a,b) = min((l,w), (w,h), (l,h), (w,l), (h,w), (h,l))
    total2 += 2*a + 2*b + l * w * h

print(area)
print(total2)
