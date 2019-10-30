-- Weak tables, defaults example

defaults = {}
-- Weakly keyed
setmetatable(defaults, {__mode = "k"})
function setDefault(t, d)
    defaults[t] = d
    setmetatable(t, {__index = function (t, _) return defaults[t] end})
end

ttab = {}
ttab['first'] = {a = 5, b = 10} -- bind table A
setDefault(ttab['first'], 0) -- enroll
for k,v in pairs(ttab['first']) do
    print(k, v)
end
print(ttab['first'].c)

ttab['first'] = {} -- A sieces use
collectgarbage()
-- defaults is empty, A has no references
for k,v in pairs(defaults) do print(k) end