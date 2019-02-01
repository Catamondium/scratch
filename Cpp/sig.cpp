#include <iostream>
#include <csignal> // signal, raise

using SIGHANDLE = void(*)(int);  // handler signature
unsigned int global;

void siginthandle(int sig)
{
	std::cout << "SIGINT(" << sig << ") recieved" << std::endl;
	std::cout << "Global: " << global << std::endl;
	exit(sig);
}

int main(int argc, char **argv)
{
	signal(SIGINT, siginthandle);
#ifdef ignore
	signal(SIGFPE, SIG_IGN);     // ignore interrupt
#endif
	raise(SIGFPE);               // gauranteed SIGFPE

	while(true) {
		if(global % 10 == 0)     // limit output
			std::cout << "looping" << std::endl;
		++global;

		if(argc > 1 && global > 100)
			raise(SIGINT);       // invoke signal internally
	}
}
