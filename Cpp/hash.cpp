#include <iostream>
#include <functional>
#include <unordered_set>

struct S {
	int x;
	int y;
	S(int x, int y): x(x), y(y) {};
};

bool operator==(const S &lhs, const S &rhs)
{
	return lhs.x == rhs.x && lhs.y == rhs.y;
}

std::ostream& operator<<(std::ostream& stream, S s)
{
	stream << "S(" << s.x << ", " << s.y << ")";
	return stream;
}

// Custom hash functor
struct MyHash {
	std::size_t operator()(S const &s) const noexcept
	{
		std::size_t xhash = std::hash<int>{}(s.x);
		std::size_t yhash = std::hash<int>{}(s.y);
		return xhash ^ (yhash << 1);
	}
};

// Specialisation form
template<>
struct std::hash<S> // is your constructor theoretically useful?
{
	std::size_t operator()(S const &s) const noexcept
	{
		std::size_t const xhash(std::hash<int>{}(s.x));
		std::size_t const yhash(std::hash<int>{}(s.y));
		return xhash ^ (yhash << 1);
	}
};

int main() {
	std::string str = "aaa";
	int num = 222;
	S vec = {2, 4};

	// Standard hashes
	// Identity hash
	std::cout << num << " -> " << std::hash<int>{}(num) << std::endl;
	std::cout << str << " -> " << std::hash<std::string>{}(str) << std::endl;
	// Custom hashes
	std::cout << vec << " -> " << std::hash<S>{}(vec) << std::endl;
	std::cout << vec << " C-> " << MyHash{}(vec) << std::endl;

#ifdef FAIL
	// Disallowed, unhashable type
	std::unordered_set<MyHash> nostuff;
#endif
	// Allowed, hashable type
	std::unordered_set<S> stuff;

	for(int i = 0; i < 3; ++i)
		stuff.insert({1, i});
	// Repeated element ignored, matched hash
	stuff.insert({1, 0}); 

	std::cout << "stuff:" << std::endl;
	for(auto &s : stuff)
		std::cout << "\t" << s << std::endl;
}
