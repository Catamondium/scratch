#include <iostream>

using namespace std;

template <class C>
struct Entity
{
	C val;
	Entity(C val_)
	{
		val = val_;
		return;
	}
};

template <typename T, typename B>
T f(T s, B m)
{
	cout << s << m << '\n';
	return s;
}

int main(void)
{
	Entity<int> var(122); // Entity<int> NOT deduced
	cout << var.val << endl;

	cout << f(222, "Ass") << endl; // f<int,string> deduced
}
