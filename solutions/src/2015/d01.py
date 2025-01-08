with open("day1.txt") as f:
    file = f.read()

def solve(part2):
    floor = 0
    for i, c in enumerate(file, 1):
        match c:
            case '(': floor += 1
            case ')': floor -= 1
        if part2 and floor == -1:
            print(i)
            break
        i+=1
    if not part2:
        print(floor)

solve(False)
solve(True)
