#include <iostream>

// implements undo()
template <typename BASE, typename T = typename BASE::value_type>
struct Undoable : BASE
{
	using value_type = T;
	T before;
	virtual void set(T v)
	{
		before = BASE::get();
		BASE::set(v);
	}
	void undo() { BASE::set(before); }
};

struct MyNum
{
	using value_type = int;
	int val;
	void set(int x) { val = x; }
	int get() { return val; }
};

int main()
{
	Undoable<MyNum> i; // 'MyNum' with undo()
	i.set(5);
	std::cout << i.get() << std::endl;
	i.set(88);
	std::cout << i.get() << std::endl;
	i.undo();
	std::cout << i.get() << std::endl;
}
