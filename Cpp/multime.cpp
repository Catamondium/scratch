#include <thread>
#include <chrono>
#include <iostream>
#include <vector>
#include <utility>


using Runner = std::pair<int, std::string> ;
std::vector<Runner> runners = 
{
	{5,  "Bob"},
	{5, "Emma"},
	{15, "Willy"}
};

std::vector<std::thread> pool;

void time(Runner run)
{
	std::this_thread::sleep_for(std::chrono::seconds(run.first));
	std::cout << "TERM:\t" << run.second << std::endl;
}

int main(int argc, char **argv)
{
	std::thread Bob([](auto a){time(a);}, runners[0]);
	std::thread Emma([](auto a){time(a);}, runners[1]);
	std::thread Willy([](auto a){time(a);}, runners[2]);

	Bob.join();
	Emma.join();
	Willy.join();
	std::cout << "main termination" << std::endl;
}
