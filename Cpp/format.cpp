#include <iostream>
#include <stdio.h>  // asprintf

struct fmt {
	std::string str;
	fmt(std::string str): str(str) {};
	template<class... Ts> std::string format(Ts...);
	template<class... Ts> std::string operator()(Ts...);
};

fmt operator""_fmt(const char * str, std::size_t)
{
	return {str};
}

template<class... Ts>
inline std::string fmt::operator()(Ts... args)
{
	char *buf;
	asprintf(&buf, str.c_str(), args...);
	std::string ret(buf);
	free(buf);

	return ret;
}


int main()
{
	std::cout << "_fmt:\t%05d"_fmt(5) << std::endl;
}
