#include <chrono>
#include <iostream>
#include <vector>
#include <utility>

#include <thread>
#include <mutex>

std::mutex cout_rights; // reserve std::cout

using timebase = std::chrono::seconds;
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
	auto before = std::chrono::system_clock::now();
	std::this_thread::sleep_for(std::chrono::seconds(run.first));

	// Aquire printing ability
	std::lock_guard<std::mutex> guard(cout_rights);
	auto elapsed = std::chrono::system_clock::now() - before;
	std::cout << "TERM:\t" << run.second
		<< "\t" << elapsed.count() / 1e9 << "s" << std::endl;
}

int main()
{
	std::thread Willy([](auto a){time(a);}, runners[2]);
	std::thread Bob([](auto a){time(a);}, runners[0]);
	std::thread Emma([](auto a){time(a);}, runners[1]);

	Bob.join();
	Emma.join();
	Willy.join();
	std::cout << "main termination" << std::endl;
}
