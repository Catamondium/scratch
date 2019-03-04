#include <iostream>

int main()
{
#ifdef __linux__ // linux
	std::cout << "Linux" << std::endl;
#endif

#ifdef _WIN32 // windows 32 || 64 bit
	std::cout << "Windows" << std::endl;
#endif
}
