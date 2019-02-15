#include <mutex>
#include <condition_variable>

// bounded semaphore ala Python
class Semaphore {
	public:
		Semaphore(int res = 0) : res(res), init(res) {};

		inline void notify()
		{
			std::unique_lock<std::mutex> lck(mtx);
			++res;
			if(res > init) throw std::logic_error("Exceeded available resources");
			cv.notify_one();
		}

		inline void wait()
		{
			std::unique_lock<std::mutex> lck(mtx);

			while(res == 0) cv.wait(lck);
			--res;
		}

	private:
		std::mutex mtx;
		std::condition_variable cv;
		int res, init;
};
