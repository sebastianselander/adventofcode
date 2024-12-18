from collections import deque
file = open("inputs/2024/18.txt").read().splitlines()

N = 71
M = 71

grid = [["." for _ in range(M)] for _ in range(N)]

seen = set()
queue = deque()
queue.append((0,0,0))
length = 0

for i in range(1024):
    col,row = file[i].split(",")
    row,col = int(row), int(col)
    grid[row][col] = "#"

while len(queue) > 0:
    c,r,cost = queue.popleft()
    print(cost)
    seen.add((r,c))
    if (r,c) == (N-1,M-1): 
        length = cost
        break
    for nr,nc in [(0,1),(1,0),(0,-1),(-1,0)]:
        r,c = r+nr, c+nc
        if (r,c) not in seen and grid[r][c] != "#" and 0 <= r < N and 0 <= c <= M:
            queue.append((r,c,cost+1))

print(length)
