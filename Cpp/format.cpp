#include <iostream>
#include <stdio.h>  // asprintf

struct fmt {
	std::string str;
	fmt(std::string str): str(str) {};
	template<class... Ts> std::string format(Ts...);
	template<class... Ts> std::string operator()(Ts...);
};

template<class... Ts>
inline std::string fmt::format(Ts... args)
{
	char *buf;
	asprintf(&buf, str.c_str(), args...);
	std::string ret(buf);
	free(buf);

	return ret;
}

template<class... Ts>
inline std::string fmt::operator()(Ts... args)
{
	return format(args...);
}

fmt operator"" _fmt(const char * str, std::size_t)
{
	return {str};
}

int main()
{
	fmt s = "ass"_fmt;
	std::cout << ".format\t\t" << "%d"_fmt.format(155) << std::endl;
	std::cout << "operator()\t" << "%05d"_fmt(5) << std::endl;
}
