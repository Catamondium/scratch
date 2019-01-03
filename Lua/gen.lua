-- Generator testing
function allwords(f)
	-- for each line in file
	for l in io.lines() do
		-- for each word
		for w in string.gfind(l, "%w+") do
			-- call f
			f(w)
		end
	end
end

allwords(print)
local count = 0
allwords(function(w)
	if w == "hellow" then count = count + 1 end
end
print(count)
