// C++2a concepts experiment
// g++-2a -std=c++2a -fconcepts

// wikipedia example

#include <iostream>

#ifdef FAILTEST // for cleaner asm
class NotComp {
}; // class lacking in equality/inequality operator
#endif

template<class T>
concept bool EqualityComparable()
{
	return requires(T a, T b) {
		{a == b} -> bool;
		{a != b} -> bool;
	};
}

// Templated for EqualityComparable types
void f(const EqualityComparable&) {} // do nothing

int main()
{
	f(122); // compiles, comparable type
#ifdef FAILTEST // control method use -DFAILTEST w/ gcc
	f(NotComp());
#endif
}
