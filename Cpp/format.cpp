#include <stdio.h>     // asprintf
#include <type_traits> // is_convertible
#include <iostream>

struct fmt {
	std::string str;
	fmt(const std::string &str) noexcept: str(str) {};
	fmt(const char *c_str) noexcept: str(c_str) {};
	template<class... Ts> std::string operator()(Ts...) noexcept;
	template<class T> fmt& operator+=(const T&) noexcept;
	template<class T> fmt operator+(const T&) const noexcept;
	operator std::string() const { return str; }
};

template<class T>
fmt& fmt::operator+=(const T &rhs) noexcept
{
	static_assert(std::is_convertible<T, fmt>::value, "Not convertible");
	this->str += fmt(rhs).str;
	return *this;
}

template<class T>
fmt fmt::operator+(const T &rhs) const noexcept
{
	static_assert(std::is_convertible<T, fmt>::value, "Not convertible");
	return fmt(*this) += fmt(rhs);
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
	std::cout << "_fmt: %05d"_fmt(5) << std::endl;

	fmt addtest = "Ass"; // const char* constructible
	addtest += "Bass";   // Template allows all types, assert requires constructible/convertible
	std::cout << "addtest: " << addtest() << std::endl;
}
