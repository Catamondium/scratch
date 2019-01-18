// g++-8 -std=c++2a -fconcepts
#include <iostream>
#include <sstream>
#include <vector>
#include <iomanip>

template<class T>
concept bool Stringable = requires(T a) {
	{std::string(a)} -> std::string;
};

template<class T>
concept bool RefStreamable = requires(T &in, std::ostream &out) {
	{operator<<(out, in)} -> std::ostream&;
};

template<class T>
concept bool valStreamable = requires(T in, std::ostream &out) {
	{operator<<(out, in)} -> std::ostream; // passed by value?
};

template<class T>
concept bool Streamable = requires {{RefStreamable<T> || valStreamable<T>};};

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
	std::string dispatch(Streamable val, auto &start, auto &end)
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
		if(*it == '%' && *(it+1) != '%') {
			out << format::dispatch(val, it, end);
			print(out, it, end, args...); // enumerate recursively
			break;
		} else if(*it == '%' && *(it+1) == '%') {
			it++;
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

#ifdef FAIL
class noStream {
int x;
};
std::ostream& operator<<(std::ostream &s, noStream &a) = delete;
#endif

int main()
{
	static_assert(! Stringable<int>, "Ints are strings!");
#ifdef FAIL
	static_assert(! Streamable<noStream>, "Function deletion is weird");
#endif

	std::cout << "_fmt: %"_fmt(5) << std::endl;
	std::cout << "bol: % %"_fmt(true, false, 555) << std::endl;
	std::cout << "escaped: %% continued"_fmt(44444) << std::endl;

	fmt addtest = "Ass"; // const char* constructible
	addtest += "Bass";   // Template allows all types, assert requires constructible/convertible
	std::cout << "addtest: " << addtest() << std::endl;

	std::vector<int> ints = {0, 1, 2, 3, 4, 5};
	std::cout << "%"_fmt(ints) << std::endl;
}
