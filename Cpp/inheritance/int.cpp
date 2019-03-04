#include <iostream>
#include <vector>

// Abstract class
// Uninstantiable
// May provide impl, interface only in this case
struct Nameable
{
	// 'virtual' gives derived types priority
	virtual std::string getName() = 0;
	virtual void setName(std::string) = 0;
};

// Must implement Nameable to be instantiable
struct S : Nameable
{
	std::string name;
	S(std::string name) : name(name){};
	std::string getName() { return "S: " + name; };
	void setName(std::string other) { name = other; };
};

struct A : Nameable
{
	std::string name;
	A(std::string name) : name(name){};
	std::string getName() { return "A: " + name; };
	void setName(std::string other) { name = other; };
};

int main()
{
	S s{"Timmy"};
	A a{"Ben"};

	// Nameable can't be allocated, but can be a casted pointer
	std::vector<Nameable *> nameables = {&s, &a};
	for (auto &n : nameables)
	{
		// getName() is available by inheritance
		std::cout << n->getName() << std::endl;
	}
}
