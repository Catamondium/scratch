#include <stdio.h>  // asprintf
#include <iostream>

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
	std::cout << "_fmt:\t%05d"_fmt(5) << std::endl;
}
