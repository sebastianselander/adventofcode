from hashlib import md5

input = "iwrupvqb"

def solve(part2):
    for i in range(0, 1000000000):
        if not part2 and "00000" == md5((input + str(i)).encode()).hexdigest()[:5]:
            print(i)
            break
        if part2 and "000000" == md5((input + str(i)).encode()).hexdigest()[:6]:
            print(i)
            break

solve(False)
solve(True)
