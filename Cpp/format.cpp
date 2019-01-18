// g++-8 -std=c++2a -fconcepts
#include <iostream>
#include <sstream>
#include <vector>
#include <iomanip>

#ifdef FAIL
class noStream {};

void /*invalid Streamable*/ operator<<(std::ostream &s, noStream a)
{
	// noop
}
#endif

template<class T>
concept bool Stringable = requires(T a) {
	{std::string(a)} -> std::string;
};

template<class T>
concept bool Streamable = requires(T a, std::ostream s) {
	{s << a} -> std::ostream;
};

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

namespace format { // Formatting endpoints for type T
	// T = Streamable generic
	template<class T>
	std::string dispatch(T val, auto &start, auto &end)
	{
		std::stringstream ss;
		ss << val; // just require regular ostream << T def
		++start; // iter presented with '%' char
		return ss.str();
	}

	// T = bool
	std::string dispatch(bool val, auto &start, auto &end)
	{
		std::stringstream ss;
		ss << std::boolalpha;
		ss << val;
		++start;
		return ss.str();
	}

	// T == std::vector<X>
	template<class X>
	std::string dispatch(std::vector<X> vec, auto &start, auto &end)
	{
		++start;
		if(vec.size() == 0)
			return "[]";
		else {
			std::string ret = "[";
			for(X &val : vec) {
				ret += "%"_fmt(val) + ", ";
			}
			return ret.substr(0, ret.size()-2) + "]";
		}
	}
}

void print(std::stringstream &out, auto &start, auto &end)
{
	for(auto it = start; it != end; ++it) {
		out << *it;
	}
}

template<class T, class... Ts>
void print(std::stringstream &out, auto start, auto end, T &val, Ts&... args)
{
	for(auto it = start; it != end; /*++it*/) {
		if(*it == '%') {
			out << format::dispatch(val, it, end);
			print(out, it, end, args...); // enumerate recursively
			break;
		}
		out << *it;
		++it;
	}
}


inline std::string fmt::operator()() noexcept
{
	return str;
}

template<class T, class... Ts>
inline std::string fmt::operator()(T val, Ts... args) noexcept
{
	std::stringstream out;
	print(out, str.begin(), str.end(), val, args...);
	return out.str();
}


int main()
{
#ifdef FAIL
	fmt failtest{112};
	std::cout << "%"_fmt(noStream{}) << std::endl;
#endif

	std::cout << "_fmt: %"_fmt(5) << std::endl;
	std::cout << "bol: % %"_fmt(true, false, 555) << std::endl;

	fmt addtest = "Ass"; // const char* constructible
	addtest += "Bass";   // Template allows all types, assert requires constructible/convertible
	std::cout << "addtest: " << addtest() << std::endl;

	std::vector<int> ints = {0, 1, 2, 3, 4, 5};
	std::cout << "%"_fmt(ints) << std::endl;
}
