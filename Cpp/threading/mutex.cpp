#include <chrono>
#include <iostream>
#include <vector>
#include <utility>

#include <thread>
#include <mutex>

std::mutex cout_rights; // reserve std::cout

using Runner = std::pair<int, std::string> ;
std::vector<Runner> runners = 
{
	{15, "Willy"},
	{5,  "Bob"},
	{5, "Emma"}
};

std::vector<std::thread> pool;

void time(Runner run)
{
	auto before = std::chrono::system_clock::now();
	std::this_thread::sleep_for(std::chrono::seconds(run.first));

	// Acquire printing ability
	std::lock_guard<std::mutex> guard(cout_rights);
	auto elapsed = std::chrono::system_clock::now() - before;
	std::cout << "TERM:\t" << run.second
		<< "\t" << elapsed.count() / 1e9 << "s" << std::endl;
}

int main()
{
	std::vector<std::thread> threads;
	for(auto &r : runners)
		threads.push_back(std::thread(
					[](auto a){time(a);}, // fails callable otherwise
					r));

	for(auto &t : threads)
		t.join();

	std::cout << "main termination" << std::endl;
}
