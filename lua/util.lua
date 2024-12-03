local M = {}
M.readfile = function(filename)
	local file = io.open(filename, "rb")
	if not file then
		return nil
	end
	local content = file:read("*all")
	file:close()
	return content
end

M.count = function(v, arr)
	local c = 0
	for i = 1, #arr do
		if arr[i] == v then
			c = c + 1
		end
		i = i + 1
	end
	return c
end

M.list = function(iterator)
    local i = 1
	local arr = {}
	for v in iterator do
		arr[i] = v
        i = i + 1
	end
	return arr
end

-- returns iterator split on newlines
M.splitlines = function(str)
	return string.gmatch(str, "[^\n]+")
end

M.splitspace = function(str)
	return string.gmatch(str, "%w+")
end

return M
