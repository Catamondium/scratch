//#include <chrono>
#include <iostream>

#include <thread>
#include <mutex>
#include <condition_variable>

std::mutex mtx;
std::condition_variable cv;
bool ready = false;
int resource = 10; // approx counted semaphore

void print()
{
	std::unique_lock<std::mutex> lck(mtx);
	while (!ready)
		cv.wait(lck); // spurious wake protection
	std::cout << "onReady" << std::endl;
}

void consumer()
{
	std::unique_lock<std::mutex> lck(mtx);
	// predicate form
	cv.wait(lck, []() { return resource != 0; });
	std::cout << resource << std::endl;
	--resource;
}

int main()
{
	std::thread onReady(print);
	std::thread consumers[10];
	for (int i = 0; i < 10; ++i)
	{
		consumers[i] = std::thread(consumer);
	}

	{ // RAII deadlock protection
		std::unique_lock<std::mutex> lck(mtx);
		ready = true;
		cv.notify_all();
	}

	while (resource > 0)
	{
		std::unique_lock<std::mutex> lck(mtx);
		cv.notify_all();
	}

	onReady.join();
	for (auto &t : consumers)
		t.join();
	std::cout << "main termination" << std::endl;
}
