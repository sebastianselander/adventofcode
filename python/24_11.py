inp = list(map(int, open("24_11.in").read().split()))

memo = {}

def blink(n, steps):
    if steps == 0:
        return 1
    if (n,steps) not in memo:
        if n == 0:
            result = blink(1, steps - 1)
        elif len(str(n)) % 2 == 0:
            number_string = str(n)
            left,right = int(s[len(number_string)//2:]), int(s[:len(number_string)//2])
            result = blink(left, steps - 1) 
            result += blink(right, steps - 1)
        else:
            result = blink(2024 * n, steps - 1)
        memo[(n,steps)] = result

    return memo[(n, steps)]

res = 0
for x in inp:
    res += blink(x,500)

print(res)
