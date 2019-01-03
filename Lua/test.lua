-- Closures, functions as objects, tables
print("Closures and first-class functions\n------")
function multiplier(x)
	return function(y)
		return x * y
	end
end

doubler = multiplier(2)
tripler = multiplier(3)

functions = {doubler, tripler} -- 'Array' indexed @ [1]

for i, v in pairs(functions) do
	print("Closure: ", i, v(2))
end

-- metatable protocol
print("\nMetatable results\n------")
dict = {
	[3] = "Fizz",
	[5] = "Buzz",
	[7] = "Baz"
}

-- Create infinite FizzBuzz seq with dict
fizzbuzz = setmetatable({}, {
	__index = function(val, n) -- Run for unknown n
		local hits = ""
		-- Unsorted by base implementation
		for k, v in pairs(dict) do
			if n % k == 0 then
				hits = hits .. v
			end
		end

		if #hits ~= 0 then
			val[n] = hits
		else
			val[n] = n
		end

		return val[n]
	end
})

for i=1,100,1 do
	print("MetaFizzBuzz: ", fizzbuzz[i])
end
