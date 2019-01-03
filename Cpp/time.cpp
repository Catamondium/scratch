#include <thread>
#include <chrono>
#include <iostream>

void time(int time, std::string name)
{
	for(int i = time; i > 0; --i) {
		std::cout << name << ":\t" << i << std::endl;
		std::this_thread::sleep_for(std::chrono::seconds(1));
	}
	std::cout << "TERM:\t" << name << std::endl;
}

int main(int argc, char **argv)
{
	int val = (argc > 1) ? std::stoi(argv[1]) : 10;
	time(val, "main");
}
