local util = require("util")
local f = util.readfile("../inputs/2024/01.txt")
local lefts = {}
local rights = {}

local i = 1

for l in util.splitlines(f) do
	local iter = util.splitspace(l)
	lefts[i] = iter()
	rights[i] = iter()
	i = i + 1
end

table.sort(lefts)
table.sort(rights)
local part1 = 0
local part2 = 0
for i = 1, #lefts do
	local v = math.abs(lefts[i] - rights[i])
	part1 = part1 + v
	part2 = part2 + (lefts[i] * util.count(lefts[i], rights))
end
print(part1)
print(part2)
print(util.list(util.splitspace("a b c d")))
