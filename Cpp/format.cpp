// g++-8 -std=c++2a -fconcepts
#include <stdio.h>     // asprintf
#include <type_traits> // is_convertible
#include <iostream>
#include <sstream>
#include <iomanip>
#include <ios>

template<class T>
concept bool Stringable = requires(T a) {
	{std::string(a)} -> std::string;
};

/*template<class T>
concept bool Streamable = requires(T a, std::ostream s) {
	{s << a};
};*/

struct fmt {
	std::string str;
	fmt(const Stringable &str) noexcept: str(str){};
	inline std::string operator()() noexcept;
	template<class T, class... Ts> std::string operator()(T, Ts...) noexcept;
	fmt& operator+=(const Stringable&) noexcept;
	fmt operator+(const Stringable&) const noexcept;
	operator std::string() const { return str; }
};

fmt& fmt::operator+=(const Stringable &rhs) noexcept
{
	this->str += std::string(rhs);
	return *this;
}

fmt fmt::operator+(const Stringable &rhs) const noexcept
{
	return fmt(*this) += std::string(rhs);
}

fmt operator""_fmt(const char * str, std::size_t) noexcept
{
	return {str};
}

inline std::string fmt::operator()() noexcept
{
	return str;
}

template<class T, class... Ts>
inline std::string fmt::operator()(T val, Ts... args) noexcept
{
	std::stringstream out;
	for(auto it = str.begin(); it != str.end(); it++) {
		if(*it == '%') {
			out << val;
			continue;
		}
		out << *it;
	}
	return out.str();
}


int main()
{
#ifdef FAIL
	fmt failtest{112}; // intentional compile error with -DFAIL
#endif

	std::cout << "_fmt: %"_fmt(5) << std::endl;
	std::cout << "bol: % %"_fmt(true, 1) << std::endl; // issue, prints true true

	fmt addtest = "Ass"; // const char* constructible
	addtest += "Bass";   // Template allows all types, assert requires constructible/convertible
	std::cout << "addtest: " << addtest() << std::endl;
}
