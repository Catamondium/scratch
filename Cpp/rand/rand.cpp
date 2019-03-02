#include <random>
#include <functional>
#include <iostream>
#include <chrono>

int main()
{
	// Time dependent seed
	std::default_random_engine generator(std::chrono::high_resolution_clock::now().time_since_epoch().count());
	// Equally probably 1..6 result
	std::uniform_int_distribution<int> distribution(1, 6);
	// Convenience bind
	auto roll = std::bind(distribution, generator);

	std::cout << roll() << " " << roll() << std::endl;
}
