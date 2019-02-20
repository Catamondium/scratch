#include <iostream>
#include <vector>

// type counting mixin
template <typename T>
struct counter {
    static int objects_created;
    static int objects_alive;

    counter()
    {
        ++objects_created;
        ++objects_alive;
    }
    
    counter(const counter&)
    {
        ++objects_created;
        ++objects_alive;
    }
	protected:
		~counter() // objects should never be removed through pointers of this type
		{
			--objects_alive;
		}
};
template <typename T> int counter<T>::objects_created(0);
template <typename T> int counter<T>::objects_alive(0);

// Classes with counter functionality
class X : counter<X> {};
class Y : counter<Y> {};

int main()
{
	// one of each alive
	X x;
	Y y;

	{
		std::vector<X> vecX(4); // 05 created overall
		std::vector<Y> vecY(9); // 10 created overall
	}

	std::cout << "X created:\t" << counter<X>::objects_created << std::endl;
	std::cout << "X alive:\t" << counter<X>::objects_alive << std::endl;

	std::cout << "Y created:\t" << counter<Y>::objects_created << std::endl;
	std::cout << "Y alive:\t" << counter<Y>::objects_alive << std::endl;
}
