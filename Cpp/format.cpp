// g++-8 -std=c++2a -fconcepts
#include <iostream>
#include <sstream>
#include <vector>
#include <iomanip>

template<class T>
concept bool Stringable = requires(T a) {
	{std::string(a)} -> std::string;
};
static_assert(! Stringable<int>, "Ints are strings!");

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

// Emulate std::hash<T>{}(thing) interface
template<class T>
struct repr {
	std::string operator()(T in)
	{
		std::stringstream ss;
		ss << in;
		return ss.str();
	}
};

template<>
struct repr<bool> {
	std::string operator()(bool bol)
	{
		std::stringstream ss;
		ss << std::boolalpha << bol;
		return ss.str();
	}
};

template<class X>
struct repr<std::vector<X>> {
	std::string operator()(std::vector<X> vec)
	{
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
};

void print(std::stringstream &out, auto &start, auto &end)
{
	for(auto it = start; it != end; ++it) {
		out << *it;
	}
}

template<class T, class... Ts>
void print(std::stringstream &out, auto start, auto end, T &val, Ts&... args)
{
	for(auto it = start; it != end;) {
		if(*it == '%') {
			if(*(it+1) != '%') {
				out << repr<T>{}(val);//format::dispatch(val, it, end);
				++it;
				print(out, it, end, args...); // enumerate recursively
				break;
			} else
				++it;
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
	std::cout << "_fmt: %"_fmt(5) << std::endl;
	std::cout << "bol: % %"_fmt(true, false, 555) << std::endl;
	std::cout << "str: %"_fmt(std::string("string")) << std::endl;
	std::cout << "escaped: %% continued"_fmt(44444) << std::endl;

	// Stringable, accepts any T -> std::string
	fmt addtest = "Ass";
	addtest += "Bass";
	std::cout << "addtest: " << addtest() << std::endl;

	std::vector<int> ints = {0, 1, 2, 3, 4, 5};
	std::cout << "%"_fmt(ints) << std::endl;
}
