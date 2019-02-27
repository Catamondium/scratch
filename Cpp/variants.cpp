#include <iostream>
#include <variant>
#include <format>
using namespace fmtf;
using var_t = std::variant<int, std::string>;

// Helper type
// Make operator() of callables available at overloaded scope
template<class... Ts> struct overloaded: Ts... {using Ts::operator()...;};
// Deduction guide
template<class... Ts> overloaded(Ts...) -> overloaded<Ts...>;

// Traditional visitor class
struct printer {
	void operator()(int x) { print("int: %\n", x); };
	void operator()(std::string x) { print("\"%\"\n", x); };
};

int main()
{
	var_t v = 10;
	// 'lambda' visitation
	std::visit(overloaded {
			[](int x) { print("int: %\n", x); },
			[](std::string x) { print("\"%\"\n", x); }
			}
			, v);

	std::visit(overloaded {
			[](int x) { print("int: %\n", x); },
			[](std::string x) { print("\"%\"\n", x); }
			}
			, v);
	v = "cats";
	// Traditional visitation
	std::visit(printer{}, v);
}
