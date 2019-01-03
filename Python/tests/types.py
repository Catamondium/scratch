# like an anonymous C struct, may be multiple types, immutable
mytuple = 'a', 255, 1.0, "biscuits", ("spam", "eggs")
print(type(mytuple))
print(mytuple)

# more explicit tuple delclaration
mytuple2 = ('b', 360, 10.0, "tea", (20, "eggs"))
print(type(mytuple2))
print(mytuple2)

# like arrays, may only store 1 type, mutable
mylist = [1, 2, 3, 4, 5]
print(type(mylist))
print(mylist)

# allows only 2 types, indexed by the left one, mutable
mydict = {1 : 'a', 2 : 'b', 3 : 'c'}
print(type(mydict))
print(mydict)
