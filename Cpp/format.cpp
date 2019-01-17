// g++-8 -std=c++2a -fconcepts
#include <stdio.h>     // asprintf
#include <type_traits> // is_convertible
#include <iostream>

template<class T>
concept bool Stringable = requires(T a) {
	{std::string(a)} -> std::string;
};

struct fmt {
	std::string str;
	fmt(const Stringable &str) noexcept: str(str){};
	template<class... Ts> std::string operator()(Ts...) noexcept;
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

template<class... Ts>
inline std::string fmt::operator()(Ts... args) noexcept
{
	char *buf;
	int err;
	do {
		err = asprintf(&buf, str.c_str(), args...);
	} while(err == -1);
	std::string ret(buf);
	free(buf);

	return ret;
}


int main()
{
#ifdef FAIL
	fmt failtest{112}; // intentional compile error with -DFAIL
#endif

	std::cout << "_fmt: %05d"_fmt(5) << std::endl;

	fmt addtest = "Ass"; // const char* constructible
	addtest += "Bass";   // Template allows all types, assert requires constructible/convertible
	std::cout << "addtest: " << addtest() << std::endl;
}
